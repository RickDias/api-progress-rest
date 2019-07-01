&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          mgdis            PROGRESS
*/
&Scoped-define WINDOW-NAME wMaintenanceNoNavigation



/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-wt-docto NO-UNDO LIKE wt-docto
       field r-Rowid as rowid.

DEF NEW GLOBAL SHARED TEMP-TABLE tt-escolhe
    FIELD linha AS INT.

def NEW GLOBAL SHARED temp-table tt-cargas
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



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wMaintenanceNoNavigation 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ES4036Z 2.09.00.001 } /*** "019001" ***/
/********************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
** performance (n∆o mexi)
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program          ES4036Z
&GLOBAL-DEFINE Version        2.09.00.000

&GLOBAL-DEFINE Folder            YES
&GLOBAL-DEFINE InitialPage       1

&GLOBAL-DEFINE FolderLabels      Itens

&GLOBAL-DEFINE ttTable           tt-wt-docto
&GLOBAL-DEFINE hDBOTable         h-bodi317
&GLOBAL-DEFINE DBOTable          wt-docto

&GLOBAL-DEFINE ttParent          
&GLOBAL-DEFINE DBOParentTable    

&GLOBAL-DEFINE page0KeyFields    
&GLOBAL-DEFINE page0Fields       tt-wt-docto.nome-abrev
&GLOBAL-DEFINE page0ParentFields 
&GLOBAL-DEFINE page1Fields       

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER prTable         AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER pcAction        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER phCaller        AS HANDLE    NO-UNDO.

/* Local Variable Definitions ---                                       */
def var h-bodi317pr          as handle no-undo.
def var h-bodi317sd          as handle no-undo.
def var h-bodi317im1bra      as handle no-undo.
def var h-bodi317va          as handle no-undo.
def var h-bodi317in          as handle no-undo.
def var c-ultimo-metodo-exec as char   no-undo.
def var l-proc-ok-aux        as log    no-undo.
def var l-altera-quantidade  as log    no-undo.
def var l-altera-preco       as log    no-undo.

/* 12/02/04 - passar cod-estabel do tt-wt-docto como parametro para es4036zb.w */
DEF SHARED VAR c-cod-estabel-esapi007a-36 LIKE estabelec.cod-estabel.
/* 17/02/04 - passar cod-emitente do tt-wt-docto como parametro para es4036zb.w */
DEF SHARED VAR c-cod-emitente-esapi007a-36 LIKE emitente.cod-emitente.
def shared var i-nr-contrato-4036 like es-contrato-arroz.nr-contrato no-undo. /* 280404 */ 
def shared VAR c-it-codigo-4036 like es-item.it-codigo no-undo. /* 191005 */ 

def new global shared var i-cod-emit-4036 like saldo-terc.cod-emitente no-undo.
def new global shared var c-nat-operacao like saldo-terc.nat-operacao no-undo.


DEF NEW GLOBAL SHARED VAR dt-trans-esapi007a-36 AS DATE. /* 011206 */ 

/* Definiá∆o da tabela tempor†ria para itens de terceiros */
/* erika 26/11/2011 */ 


/*{dibo/bodi317sd.i1}*/



{cdp/cdcfgdis.i}     /* Definicao dos pre-processadores     */

/* Definiªío da tabela temporˇria para itens de terceiros */
{esp/esbodi317sd.i1}
{esp/esbodi515.i tt-nota-fisc-adc}



/* Local Variable Definitions (DBOs Handles) --- */
DEFINE VARIABLE {&hDBOTable}  AS HANDLE NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE adm-broker-hdl AS HANDLE  NO-UNDO.
DEFINE                   VARIABLE wh-pesquisa    AS HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-implanta     AS LOGICAL.

DEFINE VARIABLE i-operacao AS INTEGER NO-UNDO.

DEF VAR de-quantidade AS DEC DECIMALS 4.

DEF VAR de-entra      AS DEC DECIMALS 3.
DEF VAR de-saida      AS DEC DECIMALS 3.
DEF VAR de-tot-saldo  AS DEC DECIMALS 3.
    
DEF VAR de-tot-secag  AS DEC DECIMALS 3.
DEF VAR de-tot-seco   AS DEC DECIMALS 3.
DEF VAR de-a-secar    AS DEC DECIMALS 3.

DEF VAR vl-tot-secag  AS DEC DECIMALS 3.
DEF VAR vl-tot-seco   AS DEC DECIMALS 3.
DEF VAR vl-a-secar    AS DEC DECIMALS 3.

DEF VAR l-erro AS LOG INIT NO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE MaintenanceNoNavigation
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0
&Scoped-define BROWSE-NAME br-tt-it-terc-nf

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-it-terc-nf

/* Definitions for BROWSE br-tt-it-terc-nf                              */
&Scoped-define FIELDS-IN-QUERY-br-tt-it-terc-nf tt-it-terc-nf.sequencia tt-it-terc-nf.it-codigo tt-it-terc-nf.cod-refer tt-it-terc-nf.quantidade tt-it-terc-nf.qt-alocada tt-it-terc-nf.qt-disponivel-inf tt-it-terc-nf.preco-total-inf tt-it-terc-nf.desc-nar   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-tt-it-terc-nf tt-it-terc-nf.qt-disponivel-inf   tt-it-terc-nf.preco-total-inf   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-tt-it-terc-nf tt-it-terc-nf
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-tt-it-terc-nf tt-it-terc-nf
&Scoped-define SELF-NAME br-tt-it-terc-nf
&Scoped-define OPEN-QUERY-br-tt-it-terc-nf OPEN QUERY {&SELF-NAME} FOR EACH tt-it-terc-nf.
&Scoped-define TABLES-IN-QUERY-br-tt-it-terc-nf tt-it-terc-nf
&Scoped-define FIRST-TABLE-IN-QUERY-br-tt-it-terc-nf tt-it-terc-nf


/* Definitions for FRAME fPage1                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fPage1 ~
    ~{&OPEN-QUERY-br-tt-it-terc-nf}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-wt-docto.nome-abrev 
&Scoped-define ENABLED-TABLES tt-wt-docto
&Scoped-define FIRST-ENABLED-TABLE tt-wt-docto
&Scoped-define DISPLAYED-TABLES tt-wt-docto
&Scoped-define FIRST-DISPLAYED-TABLE tt-wt-docto
&Scoped-Define ENABLED-OBJECTS c-serie-comp c-nro-comp c-nat-comp ~
btSelecionaNota btOK btSave btCancel btHelp rs-tipo-devolucao RECT-3 rtKeys ~
rtToolBar 
&Scoped-Define DISPLAYED-FIELDS tt-wt-docto.nome-abrev 
&Scoped-Define DISPLAYED-OBJECTS c-serie-comp c-nro-comp c-nat-comp ~
de-perc-proporcao de-perc-reajuste rs-tipo-devolucao 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wMaintenanceNoNavigation AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "&Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp 
     LABEL "&Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "&OK" 
     SIZE 10 BY 1.

DEFINE BUTTON btSave 
     LABEL "&Salvar" 
     SIZE 10 BY 1.

DEFINE BUTTON btSelecionaNota AUTO-GO 
     IMAGE-UP FILE "image\im-sav":U
     IMAGE-INSENSITIVE FILE "image\ii-sav":U
     LABEL "Seleciona Nota" 
     SIZE 5 BY 1 TOOLTIP "Mostra os itens da nota selecionada."
     FONT 4.

DEFINE VARIABLE c-nat-comp AS CHARACTER FORMAT "x(06)" 
     LABEL "Natureza do Documento":R25 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88.

DEFINE VARIABLE c-nro-comp AS CHARACTER FORMAT "x(16)" 
     LABEL "Nr Documento":R14 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88.

DEFINE VARIABLE c-serie-comp AS CHARACTER FORMAT "x(5)" 
     LABEL "SÇrie":R7 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88.

DEFINE VARIABLE de-perc-proporcao AS DECIMAL FORMAT ">>9.99<<":U INITIAL 100 
     LABEL "Proporá∆o (%)" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE de-perc-reajuste AS DECIMAL FORMAT ">>9.99<<":U INITIAL ? 
     LABEL "% de Reajuste" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE rs-tipo-devolucao AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Compra Deposito", 1,
"Secagem", 2
     SIZE 32 BY .5 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 36 BY .96.

DEFINE RECTANGLE rtKeys
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 85 BY 3.38.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 90 BY 1.42
     BGCOLOR 7 .

DEFINE BUTTON btDesmarca 
     LABEL "&Desmarca" 
     SIZE 15 BY 1.

DEFINE BUTTON btDesmarcaTodos 
     LABEL "Desma&rca Todos" 
     SIZE 15 BY 1.

DEFINE BUTTON btGeracao 
     LABEL "Geraá∆o &Automatica" 
     SIZE 15 BY 1.

DEFINE BUTTON btMarca 
     LABEL "&Marca" 
     SIZE 15 BY 1.

DEFINE BUTTON btMarcaTodos 
     LABEL "Marca &Todos" 
     SIZE 15 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-tt-it-terc-nf FOR 
      tt-it-terc-nf SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-tt-it-terc-nf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-tt-it-terc-nf wMaintenanceNoNavigation _FREEFORM
  QUERY br-tt-it-terc-nf NO-LOCK DISPLAY
      tt-it-terc-nf.sequencia         
    tt-it-terc-nf.it-codigo         
    tt-it-terc-nf.cod-refer         
    tt-it-terc-nf.quantidade   
    tt-it-terc-nf.qt-alocada        column-label "Qt Alocada"          
    tt-it-terc-nf.qt-disponivel-inf column-label "Qt Ö Alocar"  
    round(tt-it-terc-nf.qt-disponivel-inf , 5) column-label "Qt Ö Alocar" FORMAT "9999999999.9999" 
    tt-it-terc-nf.preco-total-inf        
    tt-it-terc-nf.desc-nar          
  ENABLE
    tt-it-terc-nf.qt-disponivel-inf 
    tt-it-terc-nf.preco-total-inf
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 84.72 BY 7.13
         FONT 2 ROW-HEIGHT-CHARS .46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     c-serie-comp AT ROW 3.75 COL 20 COLON-ALIGNED
     c-nro-comp AT ROW 4.75 COL 20 COLON-ALIGNED
     c-nat-comp AT ROW 2.75 COL 58 COLON-ALIGNED HELP
          "Natureza do documento de origem"
     de-perc-proporcao AT ROW 3.75 COL 58 COLON-ALIGNED
     de-perc-reajuste AT ROW 4.75 COL 58 COLON-ALIGNED
     btSelecionaNota AT ROW 4.71 COL 66.29 HELP
          "Confirma alteraá‰es"
     btOK AT ROW 16.75 COL 2
     btSave AT ROW 16.75 COL 13
     btCancel AT ROW 16.75 COL 24
     btHelp AT ROW 16.75 COL 80
     tt-wt-docto.nome-abrev AT ROW 2.75 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     rs-tipo-devolucao AT ROW 1.58 COL 29 NO-LABEL
     RECT-3 AT ROW 1.29 COL 27
     rtKeys AT ROW 2.5 COL 4
     rtToolBar AT ROW 16.54 COL 1
     "Tipo Devoluá∆o" VIEW-AS TEXT
          SIZE 15 BY .5 AT ROW 1 COL 28.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90.43 BY 17
         FONT 1.

DEFINE FRAME fPage1
     br-tt-it-terc-nf AT ROW 1.13 COL 1.14
     btMarca AT ROW 8.29 COL 1.29
     btDesmarca AT ROW 8.29 COL 16.29
     btMarcaTodos AT ROW 8.29 COL 31.29
     btDesmarcaTodos AT ROW 8.29 COL 46.29
     btGeracao AT ROW 8.29 COL 70.72 HELP
          "Geraá∆o Autom†tica"
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 7.25
         SIZE 85.14 BY 8.38
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: MaintenanceNoNavigation
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tt-wt-docto T "?" NO-UNDO mgdis wt-docto
      ADDITIONAL-FIELDS:
          field r-Rowid as rowid
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wMaintenanceNoNavigation ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 17
         WIDTH              = 90.43
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 90.43
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 90.43
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wMaintenanceNoNavigation 
/* ************************* Included-Libraries *********************** */

{maintenancenonavigation/maintenancenonavigation.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wMaintenanceNoNavigation
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME fPage1:FRAME = FRAME fpage0:HANDLE.

/* SETTINGS FOR FRAME fpage0
   FRAME-NAME Custom                                                    */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME fPage1:MOVE-AFTER-TAB-ITEM (btSelecionaNota:HANDLE IN FRAME fpage0)
       XXTABVALXX = FRAME fPage1:MOVE-BEFORE-TAB-ITEM (btOK:HANDLE IN FRAME fpage0)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FILL-IN de-perc-proporcao IN FRAME fpage0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN de-perc-reajuste IN FRAME fpage0
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME fPage1
                                                                        */
/* BROWSE-TAB br-tt-it-terc-nf 1 fPage1 */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wMaintenanceNoNavigation)
THEN wMaintenanceNoNavigation:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-tt-it-terc-nf
/* Query rebuild information for BROWSE br-tt-it-terc-nf
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-it-terc-nf.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-tt-it-terc-nf */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fpage0
/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage1
/* Query rebuild information for FRAME fPage1
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fPage1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wMaintenanceNoNavigation
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wMaintenanceNoNavigation wMaintenanceNoNavigation
ON END-ERROR OF wMaintenanceNoNavigation
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wMaintenanceNoNavigation wMaintenanceNoNavigation
ON WINDOW-CLOSE OF wMaintenanceNoNavigation
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-tt-it-terc-nf
&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME br-tt-it-terc-nf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-tt-it-terc-nf wMaintenanceNoNavigation
ON MOUSE-SELECT-DBLCLICK OF br-tt-it-terc-nf IN FRAME fPage1
DO:
    if  avail tt-it-terc-nf then
        if  not tt-it-terc-nf.selecionado then 
            apply "CHOOSE":U to btMarca in frame fPage1.
        else 
            apply "CHOOSE":U to btDesmarca in frame fPage1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-tt-it-terc-nf wMaintenanceNoNavigation
ON ROW-DISPLAY OF br-tt-it-terc-nf IN FRAME fPage1
DO:
    if  avail tt-it-terc-nf then
        if  tt-it-terc-nf.selecionado then 
            assign tt-it-terc-nf.sequencia:fgcolor            in browse br-tt-it-terc-nf = 9    
                   tt-it-terc-nf.it-codigo:fgcolor            in browse br-tt-it-terc-nf = 9
                   tt-it-terc-nf.cod-refer:fgcolor            in browse br-tt-it-terc-nf = 9
                   tt-it-terc-nf.quantidade:fgcolor           in browse br-tt-it-terc-nf = 9
                   tt-it-terc-nf.qt-alocada:fgcolor           in browse br-tt-it-terc-nf = 9
                   tt-it-terc-nf.qt-disponivel-inf:fgcolor    in browse br-tt-it-terc-nf = 9
                   tt-it-terc-nf.preco-total-inf:fgcolor      in browse br-tt-it-terc-nf = 9
                   tt-it-terc-nf.desc-nar:fgcolor             in browse br-tt-it-terc-nf = 9
                   tt-it-terc-nf.sequencia:font               in browse br-tt-it-terc-nf = 6    
                   tt-it-terc-nf.it-codigo:font               in browse br-tt-it-terc-nf = 6
                   tt-it-terc-nf.cod-refer:font               in browse br-tt-it-terc-nf = 6
                   tt-it-terc-nf.quantidade:font              in browse br-tt-it-terc-nf = 6
                   tt-it-terc-nf.qt-alocada:font              in browse br-tt-it-terc-nf = 6
                   tt-it-terc-nf.qt-disponivel-inf:font       in browse br-tt-it-terc-nf = 6
                   tt-it-terc-nf.preco-total-inf:font         in browse br-tt-it-terc-nf = 6
                   tt-it-terc-nf.desc-nar:font                in browse br-tt-it-terc-nf = 6.
        else
            assign tt-it-terc-nf.sequencia:fgcolor            in browse br-tt-it-terc-nf = 0    
                   tt-it-terc-nf.it-codigo:fgcolor            in browse br-tt-it-terc-nf = 0
                   tt-it-terc-nf.cod-refer:fgcolor            in browse br-tt-it-terc-nf = 0
                   tt-it-terc-nf.quantidade:fgcolor           in browse br-tt-it-terc-nf = 0
                   tt-it-terc-nf.qt-alocada:fgcolor           in browse br-tt-it-terc-nf = 0
                   tt-it-terc-nf.qt-disponivel-inf:fgcolor    in browse br-tt-it-terc-nf = 0
                   tt-it-terc-nf.preco-total-inf:fgcolor      in browse br-tt-it-terc-nf = 0
                   tt-it-terc-nf.desc-nar:fgcolor             in browse br-tt-it-terc-nf = 0
                   tt-it-terc-nf.sequencia:font               in browse br-tt-it-terc-nf = 2    
                   tt-it-terc-nf.it-codigo:font               in browse br-tt-it-terc-nf = 2
                   tt-it-terc-nf.cod-refer:font               in browse br-tt-it-terc-nf = 2
                   tt-it-terc-nf.quantidade:font              in browse br-tt-it-terc-nf = 2
                   tt-it-terc-nf.qt-alocada:font              in browse br-tt-it-terc-nf = 2
                   tt-it-terc-nf.qt-disponivel-inf:font       in browse br-tt-it-terc-nf = 
                                                                            if l-altera-quantidade 
                                                                            then 6
                                                                            else 2
                   tt-it-terc-nf.preco-total-inf:font         in browse br-tt-it-terc-nf = 
                                                                            if l-altera-preco 
                                                                            then 6
                                                                            else 2
                   tt-it-terc-nf.desc-nar:font                in browse br-tt-it-terc-nf = 2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel wMaintenanceNoNavigation
ON CHOOSE OF btCancel IN FRAME fpage0 /* Cancelar */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btDesmarca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDesmarca wMaintenanceNoNavigation
ON CHOOSE OF btDesmarca IN FRAME fPage1 /* Desmarca */
DO:
    if  avail tt-it-terc-nf then do:
        assign tt-it-terc-nf.selecionado = no.
        apply "ROW-DISPLAY":U to br-tt-it-terc-nf in frame fPage1.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDesmarcaTodos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDesmarcaTodos wMaintenanceNoNavigation
ON CHOOSE OF btDesmarcaTodos IN FRAME fPage1 /* Desmarca Todos */
DO:
    for each tt-it-terc-nf exclusive:
        assign tt-it-terc-nf.selecionado = no.
        br-tt-it-terc-nf:REFRESH() in frame fPage1.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btGeracao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btGeracao wMaintenanceNoNavigation
ON CHOOSE OF btGeracao IN FRAME fPage1 /* Geraá∆o Automatica */
DO:
   
   /* para produtor */
   IF tt-wt-docto.nat-operacao <> "5949MT" THEN DO:

       /* 2355743 */
       FIND FIRST ext-natur-oper-cfop NO-LOCK
            WHERE ext-natur-oper-cfop.nat-operacao = tt-wt-docto.nat-operacao NO-ERROR.

       IF NOT AVAIL ext-natur-oper-cfop
       OR    (AVAIL ext-natur-oper-cfop AND NOT ext-natur-oper-cfop.dev-mud-titularidade) THEN DO:

           /* trata para naturezas diferente de Transf.de Titularidade 14/05/2013 */
           FIND FIRST es-emitente-cei WHERE es-emitente-cei.cod-emitente = tt-wt-docto.cod-emitente NO-LOCK NO-ERROR.
           IF AVAIL es-emitente-cei AND es-emitente-cei.l-ativo = YES AND es-emitente-cei.dt-validade-cei < TODAY THEN DO:
    
    
             MESSAGE "Fornecedor " tt-wt-docto.cod-emitente " possui  Controle de CEI, porÇm a data de validade da mesma est† VENCIDA!" SKIP
               "Verifique, pois n∆o ser† permitida a sua utilizaá∆o!!!"
               VIEW-AS ALERT-BOX ERROR.
             RETURN NO-APPLY.
    
             
           END.
    
           IF (AVAIL es-emitente-cei AND es-emitente-cei.l-ativo = NO) OR NOT AVAIL es-emitente-cei THEN DO:
                MESSAGE "Fornecedor " tt-wt-docto.cod-emitente  " possui Controle de CEI INATIVO!" SKIP
                       "Confirma?"
                       VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE l-cont-cei-1 AS LOG.
                   IF l-cont-cei-1 = NO THEN RETURN NO-APPLY.
               
           END.
       END.
   END.

    ASSIGN i-operacao = int(rs-tipo-devolucao:SCREEN-VALUE IN FRAME fPage0).
      

     /*MESSAGE "antes tt-wt-docto" VIEW-AS ALERT-BOX.*/
                                                  
    /*FOR EACH tt-wt-docto NO-LOCK:
                        MESSAGE tt-wt-docto.cod-emitente
                            tt-wt-docto.cod-estabel
                            
                            VIEW-AS ALERT-BOX.

    END.*/

    RUN esp/es4036zb.w (INPUT i-operacao,
                        INPUT tt-wt-docto.cod-emitente,
                        INPUT tt-wt-docto.cod-estabel,                        
                        INPUT-OUTPUT TABLE tt-it-terc-nf).  
    /*MESSAGE "antes tt-it-terc" VIEW-AS ALERT-BOX.
   FOR EACH tt-it-terc-nf:
       MESSAGE  
     tt-it-terc-nf.sequencia         
   tt-it-terc-nf.it-codigo         
   tt-it-terc-nf.cod-refer         
   tt-it-terc-nf.quantidade   
   tt-it-terc-nf.qt-alocada        
   tt-it-terc-nf.qt-disponivel-inf 
   round(tt-it-terc-nf.qt-disponivel-inf , 5) 
   tt-it-terc-nf.preco-total-inf        
   tt-it-terc-nf.desc-nar          
 tt-it-terc-nf.qt-disponivel-inf 
   tt-it-terc-nf.preco-total-inf
           VIEW-AS ALERT-BOX.
   END.*/




    ASSIGN tt-it-terc-nf.qt-disponivel-inf:READ-ONLY IN BROWSE br-tt-it-terc-nf = YES
           tt-it-terc-nf.preco-total-inf:READ-ONLY   IN BROWSE br-tt-it-terc-nf = YES.
        
    

    OPEN QUERY br-tt-it-terc-nf FOR EACH tt-it-terc-nf.
    
    APPLY 'CHOOSE' TO btMarcaTodos IN FRAME fPage1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btHelp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp wMaintenanceNoNavigation
ON CHOOSE OF btHelp IN FRAME fpage0 /* Ajuda */
DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btMarca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btMarca wMaintenanceNoNavigation
ON CHOOSE OF btMarca IN FRAME fPage1 /* Marca */
DO:
    if  avail tt-it-terc-nf then do:
        assign tt-it-terc-nf.selecionado = yes.
        apply "ROW-DISPLAY":U to br-tt-it-terc-nf in frame fPage1.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btMarcaTodos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btMarcaTodos wMaintenanceNoNavigation
ON CHOOSE OF btMarcaTodos IN FRAME fPage1 /* Marca Todos */
DO:
    for each tt-it-terc-nf exclusive:
        assign tt-it-terc-nf.selecionado = yes.
        br-tt-it-terc-nf:REFRESH() in frame fPage1.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOK wMaintenanceNoNavigation
ON CHOOSE OF btOK IN FRAME fpage0 /* OK */
DO:

    RUN saveRecordCustom.
    IF  RETURN-VALUE = "OK":U THEN
        APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSave wMaintenanceNoNavigation
ON CHOOSE OF btSave IN FRAME fpage0 /* Salvar */
DO:
    RUN saveRecordCustom.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSelecionaNota
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSelecionaNota wMaintenanceNoNavigation
ON CHOOSE OF btSelecionaNota IN FRAME fpage0 /* Seleciona Nota */
DO:
    
    ASSIGN i-operacao = int(rs-tipo-devolucao:SCREEN-VALUE IN FRAME fPage0).

    run emptyRowErrors in h-bodi317in.
    run buscaNaturOperTpOperTerc     in h-bodi317sd(input  tt-wt-docto.nat-operacao,
                                                    output l-altera-quantidade,
                                                    output l-altera-preco).

    assign tt-it-terc-nf.qt-disponivel-inf:read-only in browse br-tt-it-terc-nf = not l-altera-quantidade
           tt-it-terc-nf.preco-total-inf:read-only   in browse br-tt-it-terc-nf = not l-altera-preco.

    RUN ValidaQuantidades (OUTPUT l-erro).

    IF  NOT l-erro THEN
        {&OPEN-QUERY-{&BROWSE-NAME}}    

    /*
    IF  NOT l-erro THEN DO:
        run geraItensTerceirosTtItTercNf in h-bodi317sd(input  tt-wt-docto.seq-wt-docto, 
                                                        input  input frame fPage0 c-serie-comp,
                                                        input  input frame fPage0 c-nro-comp,
                                                        input  input frame fPage0 c-nat-comp,
                                                        input  input frame fPage0 de-perc-proporcao,
                                                        input  input frame fPage0 de-perc-reajuste,
                                                        output table tt-it-terc-nf,
                                                        output l-proc-ok-aux).
        {&OPEN-QUERY-{&BROWSE-NAME}}    
        
        run devolveErrosbodi317sd in h-bodi317sd(output c-ultimo-metodo-exec,
                                                 output table RowErrors).
        find first RowErrors no-lock no-error.
        if  avail RowErrors then do:
            {method/ShowMessage.i1}
            {method/ShowMessage.i2 &Modal="yes"}
        end.
    END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-nat-comp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nat-comp wMaintenanceNoNavigation
ON F5 OF c-nat-comp IN FRAME fpage0 /* Natureza do Documento */
DO:


    {include/zoomvar.i &prog-zoom="inzoom/z08in404.w"
                       &campo=c-serie-comp
                       &campo2=c-nat-comp
                       &campo3=c-nro-comp
                       &campozoom=serie-docto
                       &campozoom2=nat-operacao
                       &campozoom3=nro-docto}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nat-comp wMaintenanceNoNavigation
ON MOUSE-SELECT-DBLCLICK OF c-nat-comp IN FRAME fpage0 /* Natureza do Documento */
DO:
  apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-nro-comp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nro-comp wMaintenanceNoNavigation
ON F5 OF c-nro-comp IN FRAME fpage0 /* Nr Documento */
DO:
    /*{include/zoomvar.i &prog-zoom="inzoom/z08in404.w"
                       &campo=c-serie-comp
                       &campo2=c-nat-comp
                       &campo3=c-nro-comp
                       &campozoom=serie-docto
                       &campozoom2=nat-operacao
                       &campozoom3=nro-docto}  */
                       
   /* {include/zoomvar.i &prog-zoom="eszoom/z01es306.w"
                       &campo=c-serie-comp
                       /*&campo2=c-nat-comp
                       &campo3=c-nro-comp*/
                       &campozoom=serie-docto
                       /*&campozoom2=nat-operacao
                       &campozoom3=nro-docto*/}      */      
                                           
          {include/zoomvar.i &prog-zoom="eszoom/z01es306.w"
                       &campo=c-nro-comp
                       &campozoom=nro-docto} 
END.                                                   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nro-comp wMaintenanceNoNavigation
ON MOUSE-SELECT-DBLCLICK OF c-nro-comp IN FRAME fpage0 /* Nr Documento */
DO:
    apply "F5" to self.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-serie-comp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-serie-comp wMaintenanceNoNavigation
ON F5 OF c-serie-comp IN FRAME fpage0 /* SÇrie */
DO:
    {include/zoomvar.i &prog-zoom="inzoom/z08in404.w"
                       &campo=c-serie-comp
                       &campo2=c-nat-comp
                       &campo3=c-nro-comp
                       &campozoom=serie-docto
                       &campozoom2=nat-operacao
                       &campozoom3=nro-docto}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-serie-comp wMaintenanceNoNavigation
ON MOUSE-SELECT-DBLCLICK OF c-serie-comp IN FRAME fpage0 /* SÇrie */
DO:
  apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wMaintenanceNoNavigation 


/*--- L¢gica para inicializaá∆o do programam ---*/
{maintenancenonavigation/MainBlock.i}

if  c-serie-comp:load-mouse-pointer("image/lupa.cur":U) in frame fPage0 =
    c-nro-comp:load-mouse-pointer("image/lupa.cur")     in frame fPage0 
and c-nat-comp:load-mouse-pointer("image/lupa.cur":U)   in frame fPage0 then.


ENABLE rs-tipo-devolucao 
       WITH FRAME fPage0.
ENABLE btGeracao
       WITH FRAME fPage1.

DISABLE de-perc-proporcao
        de-perc-reajuste
        WITH FRAME fPage0.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterDestroyInterface wMaintenanceNoNavigation 
PROCEDURE afterDestroyInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    {method/ShowMessage.i3}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterDisplayFields wMaintenanceNoNavigation 
PROCEDURE afterDisplayFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    disp tt-wt-docto.serie @ c-serie-comp 
         100               @ de-perc-proporcao with frame fPage0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterEnableFields wMaintenanceNoNavigation 
PROCEDURE afterEnableFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    disable tt-wt-docto.nome-abrev
            with frame fPage0.

    enable br-tt-it-terc-nf
           btMarca 
           btDesmarca 
           btMarcaTodos
           btDesmarcaTodos 
           with frame fPage1.

    enable c-serie-comp
           c-nat-comp
           c-nro-comp
           /*de-perc-proporcao
           de-perc-reajuste*/
           btSelecionaNota
           btSave
           with frame fPage0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wMaintenanceNoNavigation 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    run buscaHandleBos in phCaller(output h-bodi317in,
                                   output h-bodi317pr,
                                   output h-bodi317sd,
                                   output h-bodi317im1bra,
                                   output h-bodi317va).
    assign br-tt-it-terc-nf:NUM-LOCKED-COLUMNS in frame fPage1 = 3.

    apply "ENTRY":U to c-serie-comp in frame fPage0.

    assign i-cod-emit-4036 = tt-wt-docto.cod-emitente
           c-nat-operacao = tt-wt-docto.nat-operacao.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveParentFields wMaintenanceNoNavigation 
PROCEDURE saveParentFields :
/*------------------------------------------------------------------------------
  Purpose:     Salva valores dos campos da tabela filho ({&ttTable}) com base 
               nos campos da tabela pai ({&ttParent})
  Parameters:  
  Notes:       Este mÇtodo somente Ç executado quando a vari†vel pcAction 
               possuir os valores ADD ou COPY
------------------------------------------------------------------------------*/
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveRecordCustom wMaintenanceNoNavigation 
PROCEDURE saveRecordCustom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    def var h-boin404te as handle no-undo.

/*   MESSAGE "aqui"                         */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK. */

    /* 280404 interpreta o c¢digo do corretor do contrato 
      para impress∆o na observaá∆o da nota fiscal*/
      FIND FIRST es-contrato-arroz WHERE
                 es-contrato-arroz.nr-contrato = i-nr-contrato-4036 NO-LOCK NO-ERROR.
      IF AVAIL es-contrato-arroz THEN DO:
            
        FIND FIRST repres WHERE repres.cod-rep = es-contrato-arroz.u-int-2 NO-LOCK NO-ERROR.

      END. /* es-contrato */
      /* 280404 fim corretor na nota */

/*   MESSAGE "um"                           */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    /* para produtor */
    IF tt-wt-docto.nat-operacao <> "5949MT" THEN DO:
        /* trata para naturezas diferente de Transf.de Titularidade 14/05/2013 */
/*         MESSAGE "0"                            */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK. */

        /* 2355743 */
       FIND FIRST ext-natur-oper-cfop NO-LOCK
            WHERE ext-natur-oper-cfop.nat-operacao = tt-wt-docto.nat-operacao NO-ERROR.

       IF NOT AVAIL ext-natur-oper-cfop
       OR    (AVAIL ext-natur-oper-cfop AND NOT ext-natur-oper-cfop.dev-mud-titularidade) THEN DO:
    
            FIND FIRST emitente WHERE emitente.nome-abrev = tt-wt-docto.nome-abrev NO-LOCK NO-ERROR.
            FIND FIRST es-emitente-cei WHERE es-emitente-cei.cod-emitente = emitente.cod-emitente NO-LOCK NO-ERROR.
            IF AVAIL es-emitente-cei THEN DO: 
    
                IF es-emitente-cei.l-ativo = YES AND es-emitente-cei.dt-validade-cei < TODAY THEN DO:
                    MESSAGE "Fornecedor possui Controle de CEI Ativo, porÇm a data de validade da mesma est† VENCIDA!" SKIP
                        "Verifique, pois n∆o ser† permitida a sua utilizaá∆o!"
                        VIEW-AS ALERT-BOX ERROR.
                    RETURN NO-APPLY.
                END.
            END.
    /*         MESSAGE "a"                            */
    /*             VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    
            IF (AVAIL es-emitente-cei AND es-emitente-cei.l-ativo = NO) OR NOT AVAIL es-emitente-cei THEN DO:
               
                   MESSAGE "Fornecedor " tt-wt-docto.cod-emitente  " possui Controle de CEI INATIVO!" SKIP
                       "Confirma?"
                       VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE l-cont-cei-10 AS LOG.
                   IF l-cont-cei-10 = NO THEN RETURN NO-APPLY.
                  
               
           END.
/*        MESSAGE "b"                            */
/*            VIEW-AS ALERT-BOX INFO BUTTONS OK. */
       END.
    END.
/*     MESSAGE "oo"                           */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
      
    IF  i-operacao = 1 THEN DO:

/*         MESSAGE "oi"                           */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK. */

        FIND FIRST emitente WHERE emitente.nome-abrev = tt-wt-docto.nome-abrev NO-LOCK NO-ERROR.
      
    
        IF AVAIL repres THEN  
             ASSIGN SUBSTR(tt-wt-docto.observ-nota,1900,53) = "ARROZ A DEPOSITO - " + 
                           string(es-contrato-arroz.u-int-2) + " - " + repres.nome + "   COD.PRODUTOR: " + string(emitente.cod-emitente).

        ELSE ASSIGN SUBSTR(tt-wt-docto.observ-nota,1900,16) = "ARROZ A DEPOSITO" + "   COD.PRODUTOR: " + string(emitente.cod-emitente).
/* MESSAGE "dep"                          */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
        

    END. /* operacao 1 */
    ELSE
        IF  i-operacao = 2 THEN DO:

/*             MESSAGE "2"                            */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK. */
            
            IF AVAIL repres THEN  
              ASSIGN SUBSTR(tt-wt-docto.observ-nota,1900,44) = "SECAGEM - " + 
                           string(es-contrato-arroz.u-int-2) + " - " + repres.nome + "   COD.PRODUTOR: " + string(emitente.cod-emitente). /* 280404 */
                                                                                   
            ELSE ASSIGN SUBSTR(tt-wt-docto.observ-nota,1900,7) = "SECAGEM" + "   COD.PRODUTOR: " + string(emitente.cod-emitente).
/*             MESSAGE "colocou que Ç de secagem na observaá∆o da NF" VIEW-AS ALERT-BOX. */
        END. /* operacao 2 */
        ELSE DO:                                                                                
            IF AVAIL repres THEN  
              ASSIGN SUBSTR(tt-wt-docto.observ-nota,1900,37) = string(es-contrato-arroz.u-int-2) + " - " + 
                            repres.nome + "   COD.PRODUTOR: " + string(emitente.cod-emitente). /* 280404 */
            ELSE ASSIGN SUBSTR(tt-wt-docto.observ-nota,1900,100) =  "" .
        END. /* outras operacoes */
/* MESSAGE "sai"                          */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
        
    FIND wt-docto
        WHERE wt-docto.seq-wt-docto = tt-wt-docto.seq-wt-docto
        NO-ERROR.
    IF  AVAIL wt-docto THEN
        ASSIGN wt-docto.observ-nota = tt-wt-docto.observ-nota
        
               wt-docto.no-ab-reppri = "032 - CASA" /* 30/11/2011 para colcoar automaricamente o casa - solic.Mauricio */.
/* MESSAGE "pass"                         */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */

  
    run emptyRowErrors in h-bodi317in.
   
    run inbo/boin404te.p persistent set h-boin404te.
    
    run setaHandleBoin404te in h-bodi317sd(input h-boin404te).
    


    run geraWtItDoctoPartindoDoTtItTercNf in h-bodi317sd(input  tt-wt-docto.seq-wt-docto,
                                                         input  10,
                                                         input  10,
                                                         input  table tt-it-terc-nf,
                                                         output l-proc-ok-aux).

    
    
    run devolveErrosbodi317sd             in h-bodi317sd(output c-ultimo-metodo-exec,
                                                         output table RowErrors).
    delete procedure h-boin404te.
    ASSIGN h-boin404te = ?.

    find first RowErrors no-lock no-error.
    if  avail RowErrors then do:
        {method/ShowMessage.i1}
        {method/ShowMessage.i2 &Modal="yes"}
    end.
    


/*        MESSAGE "dep"                          */
/*            VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    
   apply "ENTRY":U to c-serie-comp in frame fPage0.
   
   if  l-proc-ok-aux then do:
        for each tt-it-terc-nf exclusive:
            delete tt-it-terc-nf.
        end.

        {&OPEN-QUERY-{&BROWSE-NAME}}    

        RUN goToRecord2 IN phCaller (INPUT tt-wt-docto.seq-wt-docto).

        return "OK":U.
    end.
    else 
        return "NOK":U.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidaQuantidades wMaintenanceNoNavigation 
PROCEDURE ValidaQuantidades :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF OUTPUT PARAM p-erro AS LOG.

DEF BUFFER b-tt-it-terc-nf FOR tt-it-terc-nf.
DEF BUFFER b-componente    FOR componente.

DEF VAR i-sequencia  AS INT.

DEF VAR de-quant-dev AS DEC DECIMALS 3.
DEF VAR de-quant-aux AS DEC DECIMALS 3.

DEF VAR de-tot-a-secar  AS DEC DECIMALS 3.
DEF VAR de-tot-qt-saldo AS DEC DECIMALS 3.



FIND nota-fiscal
    WHERE nota-fiscal.cod-estabel = tt-wt-docto.cod-estabel
      AND nota-fiscal.serie       = INPUT FRAME fPage0 c-serie-comp 
      AND nota-fiscal.nr-nota-fis = INPUT FRAME fPage0 c-nro-comp   
    NO-LOCK NO-ERROR.
IF  NOT AVAIL nota-fiscal THEN DO:
    MESSAGE "Nota Fiscal nao cadastrada"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ASSIGN p-erro = YES.
    APPLY 'entry' TO c-nro-comp IN FRAME fPage0.
    RETURN NO-APPLY.
END.
IF  nota-fiscal.dt-cancela <> ? THEN DO:
    MESSAGE "Nota Fiscal est† cancelada"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ASSIGN p-erro = YES.
    APPLY 'entry' TO c-nro-comp IN FRAME fPage0.
    RETURN NO-APPLY.
END.


ASSIGN de-quantidade = 0.

FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK
    BREAK BY it-nota-fisc.it-codigo:

    IF  FIRST-OF(it-nota-fisc.it-codigo) THEN
        ASSIGN de-quantidade = 0.

    ASSIGN de-quantidade = de-quantidade + it-nota-fisc.qt-faturada[1].

    Validation-block:
    FOR EACH saldo-terc USE-INDEX documento
          WHERE saldo-terc.serie-docto   = INPUT FRAME fPage0 c-serie-comp AND
                saldo-terc.nro-docto     = INPUT FRAME fPage0 c-nro-comp   AND
                saldo-terc.cod-emitente  = tt-wt-docto.cod-emitente        AND
                saldo-terc.nat-operacao  = INPUT FRAME fPage0 c-nat-comp   AND
                saldo-terc.it-codigo     = it-nota-fisc.it-codigo          NO-LOCK
          BY saldo-terc.nro-docto
          BY saldo-terc.nat-operacao
          BY saldo-terc.it-codigo:  


        IF saldo-terc.quantidade < 0 THEN NEXT.
        
        IF saldo-terc.tipo-sal-terc <> 5 THEN NEXT.  /* ENTRADA CONSIGNACAO */ 

        /*    
        FIND FIRST es-movto-arroz USE-INDEX ch-item-doc-est
            WHERE es-movto-arroz.serie-docto  = saldo-terc.serie-docto   AND
                  es-movto-arroz.nro-docto    = saldo-terc.nro-docto     AND
                  es-movto-arroz.cod-emitente = saldo-terc.cod-emitente  AND
                  es-movto-arroz.nat-operacao = saldo-terc.nat-operacao  AND
                  es-movto-arroz.it-codigo    = saldo-terc.it-codigo     AND 
                  SUBSTR(es-movto-arroz.tp-desconto,4,7) = "SECAGEM"       
            NO-LOCK NO-ERROR.
    
        IF  AVAIL es-movto-arroz THEN DO:
            ASSIGN de-tot-a-secar = de-tot-a-secar + es-movto-arroz.quantidade.
            RELEASE es-movto-arroz.
        END.
        */
        FIND FIRST es-movto-arroz USE-INDEX ch-item-doc-est
            WHERE es-movto-arroz.serie-docto  = saldo-terc.serie-docto   AND
                  es-movto-arroz.nro-docto    = saldo-terc.nro-docto     AND
                  es-movto-arroz.cod-emitente = saldo-terc.cod-emitente  AND
                  es-movto-arroz.nat-operacao = saldo-terc.nat-operacao  AND /*
                  es-movto-arroz.it-codigo    = saldo-terc.it-codigo     AND */  
                  SUBSTR(es-movto-arroz.tp-desconto,4,7) <> "SECAGEM"
            NO-LOCK NO-ERROR.
        IF  AVAIL es-movto-arroz THEN DO:
            ASSIGN de-a-secar = 0.
            RUN esp/esapi007.p (INPUT  es-movto-arroz.nr-contrato,
                                INPUT  es-movto-arroz.nr-ticket,
                                INPUT  es-movto-arroz.nr-ticket,
                                OUTPUT de-entra,    
                                OUTPUT de-saida,    
                                OUTPUT de-tot-saldo,
                                OUTPUT de-tot-secag,
                                OUTPUT de-tot-seco, 
                                OUTPUT de-a-secar,
                                OUTPUT vl-tot-secag,
                                OUTPUT vl-tot-seco, 
                                OUTPUT vl-a-secar).
            ASSIGN de-tot-qt-saldo = de-tot-qt-saldo + de-tot-saldo
                   de-tot-a-secar  = de-tot-a-secar + de-a-secar.
            RELEASE es-movto-arroz.
        END.
    END.
    
    ASSIGN de-tot-qt-saldo = de-tot-qt-saldo - de-tot-a-secar.
    
    
    IF  i-operacao = 1 THEN DO:
        ASSIGN de-quantidade = de-tot-qt-saldo.
        
        IF  de-quantidade > de-tot-qt-saldo THEN DO:
            MESSAGE "Quantidade da Nota n∆o pode ser superior ao saldo total a dep¢sito: " + STRING(de-tot-qt-saldo)
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            ASSIGN p-erro = YES.
            APPLY 'entry' TO c-nro-comp IN FRAME fPage0.
            RETURN NO-APPLY.
        END.
        
    END.
    ELSE DO:
        ASSIGN de-quantidade = de-tot-a-secar.
        
        IF  de-quantidade > de-tot-a-secar THEN DO:
            MESSAGE "Quantidade da Nota n∆o pode ser superior ao saldo total de secagem: " + STRING(de-tot-a-secar)
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            ASSIGN p-erro = YES.
            APPLY 'entry' TO c-nro-comp IN FRAME fPage0.
            RETURN NO-APPLY.
        END.
    END.


    ASSIGN de-quant-aux = de-quantidade.

    Cria-temp-table:
    FOR EACH saldo-terc USE-INDEX documento
          WHERE saldo-terc.serie-docto   = INPUT FRAME fPage0 c-serie-comp AND
                saldo-terc.nro-docto     = INPUT FRAME fPage0 c-nro-comp   AND
                saldo-terc.cod-emitente  = tt-wt-docto.cod-emitente        AND
                saldo-terc.nat-operacao  = INPUT FRAME fPage0 c-nat-comp   AND
                saldo-terc.it-codigo     = it-nota-fisc.it-codigo          NO-LOCK     
          BY saldo-terc.nro-docto
          BY saldo-terc.nat-operacao
          BY saldo-terc.it-codigo:  


        IF saldo-terc.quantidade < 0 THEN NEXT.
        
        IF saldo-terc.tipo-sal-terc <> 5 THEN NEXT.  /* ENTRADA CONSIGNACAO */ 

        FIND FIRST es-movto-arroz USE-INDEX ch-item-doc-est
            WHERE es-movto-arroz.serie-docto  = saldo-terc.serie-docto   AND
                  es-movto-arroz.nro-docto    = saldo-terc.nro-docto     AND
                  es-movto-arroz.cod-emitente = saldo-terc.cod-emitente  AND
                  es-movto-arroz.nat-operacao = saldo-terc.nat-operacao  AND /*
                  es-movto-arroz.it-codigo    = saldo-terc.it-codigo     AND */  
                  SUBSTR(es-movto-arroz.tp-desconto,4,7) <> "SECAGEM"
            NO-LOCK NO-ERROR.
        IF  AVAIL es-movto-arroz THEN DO:
            ASSIGN de-a-secar = 0.
            RUN esp/esapi007.p (INPUT  es-movto-arroz.nr-contrato,
                                INPUT  es-movto-arroz.nr-ticket,
                                INPUT  es-movto-arroz.nr-ticket,
                                OUTPUT de-entra,    
                                OUTPUT de-saida,    
                                OUTPUT de-tot-saldo,
                                OUTPUT de-tot-secag,
                                OUTPUT de-tot-seco, 
                                OUTPUT de-a-secar,
                                OUTPUT vl-tot-secag,
                                OUTPUT vl-tot-seco, 
                                OUTPUT vl-a-secar).
            ASSIGN de-tot-qt-saldo = de-tot-qt-saldo + de-tot-saldo
                   de-tot-a-secar  = de-tot-a-secar + de-a-secar.
            RELEASE es-movto-arroz.
        END.
    
        ASSIGN de-tot-qt-saldo = de-tot-qt-saldo - de-tot-a-secar.
    
        FOR FIRST componente
           /* erika saldo-terc.nro-docto
          BY saldo-terc.nat-operacao
          BY saldo-terc.it-codigo:  

        IF saldo-terc.quantidade    < 0 THEN NEXT.
        IF saldo-terc.tipo-sal-terc <> 5  THEN next. /* ENTRADA CONSIGNACAO */ 
        

        FIND FIFIELDS(componente.preco-total[1]
                   componente.cod-emitente 
                   componente.nro-docto    
                   componente.serie-docto  
                   componente.nat-operacao 
                   componente.it-codigo    
                   componente.cod-refer   
                   componente.sequencia)*/
            WHERE componente.serie-docto  = saldo-terc.serie-docto
            AND   componente.nro-docto    = saldo-terc.nro-docto
            AND   componente.cod-emitente = saldo-terc.cod-emitente
            AND   componente.nat-operacao = saldo-terc.nat-operacao
            AND   componente.it-codigo    = saldo-terc.it-codigo
            AND   componente.cod-refer    = saldo-terc.cod-refer
            AND   componente.sequencia    = saldo-terc.sequencia NO-LOCK:
    
            FIND ITEM
                WHERE ITEM.it-codigo = saldo-terc.it-codigo
                NO-LOCK NO-ERROR.
            IF  NOT AVAIL ITEM THEN
                NEXT Cria-temp-table.
    
            /*MESSAGE "ANTES" SKIP
                    "quantidade " saldo-terc.quantidade SKIP                   
                    "dec-1: " saldo-terc.dec-1          SKIP               
                    "saldo-terc.quantidade - saldo-terc.dec-1 " saldo-terc.quantidade - saldo-terc.dec-1 SKIP
                    "DE-QUANT-DEV: " DE-QUANT-DEV SKIP
                    "de-quant-aux " de-quant-aux                                         
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    
            IF  i-operacao = 1 THEN DO:
                IF  (saldo-terc.quantidade - saldo-terc.dec-1) >= de-quantidade - (de-quantidade - de-quant-dev) THEN
                    ASSIGN de-quant-aux = de-quantidade - (de-quantidade - de-quant-dev)
                           de-quant-dev = de-quant-dev  - (de-quantidade - (de-quantidade - de-quant-dev)).
                ELSE
                    ASSIGN de-quant-aux = (saldo-terc.quantidade - saldo-terc.dec-1)
                           de-quant-dev = de-quant-dev - (saldo-terc.quantidade - saldo-terc.dec-1).
            END.
            ELSE DO:
                IF  de-a-secar >= de-quantidade - (de-quantidade - de-quant-dev) THEN 
                    ASSIGN de-quant-aux = de-quantidade - (de-quantidade - de-quant-dev)
                           de-quant-dev = de-quant-dev  - de-quant-aux.
                ELSE
                    ASSIGN de-quant-aux = de-a-secar
                           de-quant-dev = de-quant-dev - de-a-secar.
            END.
    
/*                                                                                                               */
/*             MESSAGE "CRIA TT-IT-TERC-NF" SKIP                                                                 */
/*                     "quantidade " saldo-terc.quantidade SKIP                                                  */
/*                     "dec-1: " saldo-terc.dec-1          SKIP                                                  */
/*                     "saldo-terc.quantidade - saldo-terc.dec-1 " saldo-terc.quantidade - saldo-terc.dec-1 SKIP */
/*                     "DE-QUANT-DEV: " DE-QUANT-DEV SKIP                                                        */
/*                     "de-quant-aux " de-quant-aux      SKIP(2)                                                 */
/*                     "tt-it-terc-nf.qt-disponivel-inf" de-quantidade                                           */
/*                    VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                         */
/*                                                                                                               */
    
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
                                                         THEN saldo-terc.quantidade
                                                         ELSE de-tot-secag   
                   tt-it-terc-nf.qt-alocada        = IF  i-operacao = 1  
                                                         THEN saldo-terc.dec-1
                                                         ELSE de-tot-seco  
                   tt-it-terc-nf.qt-disponivel     = IF  i-operacao = 1 
                                                         THEN (saldo-terc.quantidade - saldo-terc.dec-1)
                                                         ELSE (de-tot-secag - de-tot-seco)   
                   tt-it-terc-nf.qt-disponivel-inf = de-quantidade
                   tt-it-terc-nf.preco-total       = componente.preco-total[1]
                   tt-it-terc-nf.selecionado       = NO.
    
            FOR EACH b-componente
                /*
                FIELDS (b-componente.componente
                        b-componente.preco-total[1]
                        b-componente.desconto[1])
                        */
                WHERE b-componente.cod-emitente = componente.cod-emitente 
                AND   b-componente.nro-comp     = componente.nro-docto    
                AND   b-componente.serie-comp   = componente.serie-docto  
                AND   b-componente.nat-comp     = componente.nat-operacao 
                AND   b-componente.it-codigo    = componente.it-codigo    
                AND   b-componente.cod-refer    = componente.cod-refer   
                AND   b-componente.seq-comp     = componente.sequencia 
                AND   (   b-componente.quantidade > 0
                       OR b-componente.dt-retorno = wt-docto.dt-trans) NO-LOCK: /* NF Reajuste ate Dt Trans */ 
        
                IF  b-componente.componente = 1 then /* Envio */
                    ASSIGN tt-it-terc-nf.preco-total = tt-it-terc-nf.preco-total + b-componente.preco-total[1].
                ELSE /* Retorno */
                    ASSIGN tt-it-terc-nf.preco-total = tt-it-terc-nf.preco-total - b-componente.preco-total[1].
            END.
    
            /*
            IF  natur-oper.tp-oper-terc = 6 THEN /* Reajuste de preªo */
                ASSIGN tt-it-terc-nf.preco-total = tt-it-terc-nf.preco-total * p-de-reajuste / 100.
             */
              
            ASSIGN tt-it-terc-nf.preco-total-inf = tt-it-terc-nf.preco-total.
    
            IF  de-quant-dev <= 0 THEN
                LEAVE Cria-temp-table.
        END.
    END.

END.

{&OPEN-QUERY-{&BROWSE-NAME}} 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


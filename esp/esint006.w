&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

{include/i-prgvrs.i esint006 9.99.99.999}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i esint006 <m¢dulo>}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */


DEF TEMP-TABLE tt-movto NO-UNDO
    FIELD transacao    AS CHARACTER COLUMN-LABEL "Transa‡Æo"
    FIELD ind-tipo-trans LIKE es-api-param.ind-tipo-trans
    FIELD id-movto  AS INTEGER                column-label "Id Movto"
    FIELD cd-tipo   AS INTEGER 
    FIELD chave     AS CHARACTER format 'X(30)' column-label "Chave"
    FIELD dt-inicio AS DATETIME      column-label "Data In¡cio Processo"
    FIELD dt-fim    AS DATETIME      column-label "Data T‚rmino Processo"
    FIELD cd-status AS CHARACTER COLUMN-LABEL "Status" FORMAT 'X(20)'  
    FIELD des-tipo-integr LIKE es-api-param.des-tipo-integr
    FIELD cd-tipo-integr AS INTEGER
    INDEX i1 IS PRIMARY UNIQUE id-movto cd-tipo.

DEF TEMP-TABLE tt-log NO-UNDO
    FIELD id-movto AS INTEGER                  column-label "Id Movto"
    FIELD cd-tipo  AS INTEGER column-label "Tipo Movto"
    FIELD nr-seq   AS INTEGER                  column-label "Seq"
    FIELD c-log    AS CHARACTER format "X(250)"   column-label "Log"
    FIELD data     AS DATETIME  COLUMN-LABEL "Data"
    INDEX i1 IS PRIMARY UNIQUE id-movto cd-tipo nr-seq.

{esp\esint001rp.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-log

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-log tt-movto

/* Definitions for BROWSE br-log                                        */
&Scoped-define FIELDS-IN-QUERY-br-log tt-log.id-movto tt-log.data tt-log.cd-tipo tt-log.nr-seq tt-log.c-log   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-log   
&Scoped-define SELF-NAME br-log
&Scoped-define QUERY-STRING-br-log FOR EACH tt-log
&Scoped-define OPEN-QUERY-br-log OPEN QUERY {&SELF-NAME} FOR EACH tt-log.
&Scoped-define TABLES-IN-QUERY-br-log tt-log
&Scoped-define FIRST-TABLE-IN-QUERY-br-log tt-log


/* Definitions for BROWSE br-movto                                      */
&Scoped-define FIELDS-IN-QUERY-br-movto tt-movto.transacao tt-movto.id-movto tt-movto.des-tipo-integr tt-movto.chave tt-movto.dt-inicio tt-movto.dt-fim tt-movto.cd-status   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-movto   
&Scoped-define SELF-NAME br-movto
&Scoped-define QUERY-STRING-br-movto FOR EACH tt-movto
&Scoped-define OPEN-QUERY-br-movto OPEN QUERY {&SELF-NAME} FOR EACH tt-movto.
&Scoped-define TABLES-IN-QUERY-br-movto tt-movto
&Scoped-define FIRST-TABLE-IN-QUERY-br-movto tt-movto


/* Definitions for FRAME f-cad                                          */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-rep rt-button RECT-4 RECT-5 cb-sistema ~
cb-tipo-integr rs-transacao bt-pesquisar cb-estado bt-sla cb-status ~
fi-dt-movto bt-rel fi-chave bt-agend bt-faixa br-movto br-log 
&Scoped-Define DISPLAYED-OBJECTS cb-sistema cb-tipo-integr rs-transacao ~
cb-estado cb-status fi-dt-movto fi-chave 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-livre AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-programa 
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-livre MENUBAR
       SUB-MENU  mi-programa    LABEL "&Nome-do-Programa"
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .

DEFINE MENU POPUP-MENU-br-movto 
       MENU-ITEM m_Copia_Chave_para_rea_de_tra LABEL "Copia Chave para  rea de transferˆncia"
       MENU-ITEM m_Visualiza_Json LABEL "Visualiza Json"
       MENU-ITEM m_Visualiza_Retorno LABEL "Visualiza Retorno".


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-agend 
     IMAGE-UP FILE "image/im-agend.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Agendamento" 
     SIZE 4.14 BY 1.13 TOOLTIP "Agendamento de Execu‡Æo".

DEFINE BUTTON bt-faixa 
     IMAGE-UP FILE "image/im-ran.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Faixa" 
     SIZE 4.14 BY 1.13 TOOLTIP "Faixa".

DEFINE BUTTON bt-pesquisar 
     IMAGE-UP FILE "image/toolbar/im-sea1.bmp":U
     LABEL "Pesquisar" 
     SIZE 4.57 BY 1.21.

DEFINE BUTTON bt-rel 
     IMAGE-UP FILE "image/im-solic.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Relat¢rio" 
     SIZE 4.14 BY 1.13 TOOLTIP "Relat¢rio Integra‡äes".

DEFINE BUTTON bt-rep 
     IMAGE-UP FILE "image/toolbar/im-tick.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Reprocessar" 
     SIZE 4.14 BY 1.13 TOOLTIP "Reprocessar".

DEFINE BUTTON bt-sla 
     IMAGE-UP FILE "image/im-agen1.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "SLA" 
     SIZE 4.14 BY 1.13.

DEFINE VARIABLE cb-estado AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 99 
     LABEL "Estado" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Todos",99,
                     "Pendente",0,
                     "Processando",1,
                     "Processado",2
     DROP-DOWN-LIST
     SIZE 13.29 BY 1 NO-UNDO.

DEFINE VARIABLE cb-sistema AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 99 
     LABEL "Sistema" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Todas",99
     DROP-DOWN-LIST
     SIZE 25 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cb-status AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 99 
     LABEL "Status" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Todos",99,
                     "NÆo Verificado",0,
                     "Integrado (OK)",1,
                     "NÆo Integrado (Erro)",2
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cb-tipo-integr AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 99 
     LABEL "Tipo Integra‡Æo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Todas",99
     DROP-DOWN-LIST
     SIZE 25 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi-chave AS CHARACTER FORMAT "X(256)":U 
     LABEL "Chave" 
     VIEW-AS FILL-IN 
     SIZE 27 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-movto AS DATE FORMAT "99/99/9999":U INITIAL 01/01/01 
     LABEL "Data Movto" 
     VIEW-AS FILL-IN 
     SIZE 9.86 BY .88 NO-UNDO.

DEFINE VARIABLE rs-transacao AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Todas", 1,
"Importa‡Æo", 2,
"Exporta‡Æo", 3
     SIZE 32.43 BY .63 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 111 BY 1.83.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 111 BY 1.25.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 111 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-log FOR 
      tt-log SCROLLING.

DEFINE QUERY br-movto FOR 
      tt-movto SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-log
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-log w-livre _FREEFORM
  QUERY br-log DISPLAY
      tt-log.id-movto 
tt-log.data
tt-log.cd-tipo  
tt-log.nr-seq   
tt-log.c-log FORMAT 'x(250)'
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 111 BY 4.17
         FONT 1.

DEFINE BROWSE br-movto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-movto w-livre _FREEFORM
  QUERY br-movto DISPLAY
      tt-movto.transacao FORMAT 'X(15)'
tt-movto.id-movto  
tt-movto.des-tipo-integr FORMAT 'X(30)'   
tt-movto.chave     
tt-movto.dt-inicio COLUMN-LABEL "Data In¡cio"
tt-movto.dt-fim    COLUMN-LABEL "Data T‚rmino"
tt-movto.cd-status FORMAT 'X(40)'
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 111 BY 11.75
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-rep AT ROW 1.21 COL 1.86 WIDGET-ID 18
     cb-sistema AT ROW 3.13 COL 46.86 COLON-ALIGNED WIDGET-ID 48
     cb-tipo-integr AT ROW 3.13 COL 83.72 COLON-ALIGNED WIDGET-ID 10
     rs-transacao AT ROW 3.25 COL 3.57 NO-LABEL WIDGET-ID 30
     bt-pesquisar AT ROW 4.79 COL 105.72 WIDGET-ID 16
     cb-estado AT ROW 4.88 COL 7.29 COLON-ALIGNED WIDGET-ID 8
     bt-sla AT ROW 1.21 COL 19.57 WIDGET-ID 26
     cb-status AT ROW 4.92 COL 27 COLON-ALIGNED WIDGET-ID 12
     fi-dt-movto AT ROW 4.92 COL 56.29 COLON-ALIGNED WIDGET-ID 46
     bt-rel AT ROW 1.21 COL 15.14 WIDGET-ID 24
     fi-chave AT ROW 4.92 COL 76.14 COLON-ALIGNED WIDGET-ID 14
     bt-agend AT ROW 1.21 COL 10.72 WIDGET-ID 22
     bt-faixa AT ROW 1.21 COL 6.29 WIDGET-ID 20
     br-movto AT ROW 6.5 COL 1 WIDGET-ID 200
     br-log AT ROW 18.38 COL 1 WIDGET-ID 300
     " Filtros" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 4.25 COL 2.29 WIDGET-ID 44
     " Transa‡Æo" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 2.58 COL 2.72 WIDGET-ID 42
     rt-button AT ROW 1 COL 1
     RECT-4 AT ROW 4.42 COL 1 WIDGET-ID 38
     RECT-5 AT ROW 2.92 COL 1.14 WIDGET-ID 40
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 162 BY 22.7
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-livre
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-livre ASSIGN
         HIDDEN             = YES
         TITLE              = "Monitor de Integra‡äes"
         HEIGHT             = 21.75
         WIDTH              = 111.72
         MAX-HEIGHT         = 27.75
         MAX-WIDTH          = 162
         VIRTUAL-HEIGHT     = 27.75
         VIRTUAL-WIDTH      = 162
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-livre:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-livre 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-livre.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-livre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
/* BROWSE-TAB br-movto bt-faixa f-cad */
/* BROWSE-TAB br-log br-movto f-cad */
ASSIGN 
       br-movto:POPUP-MENU IN FRAME f-cad             = MENU POPUP-MENU-br-movto:HANDLE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-log
/* Query rebuild information for BROWSE br-log
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-log.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE br-log */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-movto
/* Query rebuild information for BROWSE br-movto
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-movto.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE br-movto */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Monitor de Integra‡äes */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Monitor de Integra‡äes */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-movto
&Scoped-define SELF-NAME br-movto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-movto w-livre
ON ROW-DISPLAY OF br-movto IN FRAME f-cad
DO:

        IF tt-movto.cd-status = "NÆo Verificado" THEN
            ASSIGN tt-movto.transacao:fgcolor in browse br-movto = 1
                   tt-movto.id-movto :fgcolor in browse br-movto = 1
                   tt-movto.des-tipo-integr   :fgcolor in browse br-movto = 1
                   tt-movto.chave     :fgcolor in browse br-movto = 1
                   tt-movto.dt-inicio :fgcolor in browse br-movto = 1
                   tt-movto.dt-fim    :fgcolor in browse br-movto = 1
                   tt-movto.cd-status :fgcolor in browse br-movto = 1.   
               
        ELSE IF tt-movto.cd-status = "Integrado (OK)" THEN
             ASSIGN tt-movto.transacao:fgcolor in browse br-movto = 9
                    tt-movto.id-movto :fgcolor in browse br-movto = 9
                    tt-movto.des-tipo-integr  :fgcolor in browse br-movto = 9
                    tt-movto.chave    :fgcolor in browse br-movto = 9
                    tt-movto.dt-inicio:fgcolor in browse br-movto = 9
                    tt-movto.dt-fim   :fgcolor in browse br-movto = 9
                    tt-movto.cd-status:fgcolor in browse br-movto = 9.
                
            ELSE IF tt-movto.cd-status = "NÆo Integrado (Erro)" THEN
                ASSIGN tt-movto.transacao:fgcolor in browse br-movto = 12
                       tt-movto.id-movto :fgcolor in browse br-movto = 12
                       tt-movto.des-tipo-integr  :fgcolor in browse br-movto = 12
                       tt-movto.chave    :fgcolor in browse br-movto = 12
                       tt-movto.dt-inicio:fgcolor in browse br-movto = 12
                       tt-movto.dt-fim   :fgcolor in browse br-movto = 12
                       tt-movto.cd-status:fgcolor in browse br-movto = 12.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-movto w-livre
ON VALUE-CHANGED OF br-movto IN FRAME f-cad
DO:
  
    OPEN QUERY br-log
        FOR EACH tt-log OF tt-movto BY nr-seq DESC.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-pesquisar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-pesquisar w-livre
ON CHOOSE OF bt-pesquisar IN FRAME f-cad /* Pesquisar */
DO:                                                             
    ASSIGN FRAME {&FRAME-NAME} rs-transacao cb-status cb-estado cb-sistema fi-chave cb-tipo-integr.

    EMPTY TEMP-TABLE tt-movto.
    EMPTY TEMP-TABLE tt-log.

    SESSION:SET-WAIT-STATE("General").
        
    IF rs-transacao = 1 OR rs-transacao = 2 THEN DO:        

        FOR EACH es-api-import NO-LOCK WHERE DATE(es-api-import.data-movto) = DATE(fi-dt-movto:SCREEN-VALUE IN FRAME {&FRAME-NAME}):

            IF cb-tipo-integr <> 99 AND es-api-import.cd-tipo-integr <> cb-tipo-integr then NEXT.
            IF cb-status      <> 99 and es-api-import.cod-status     <> cb-status      then NEXT.
            IF cb-estado      <> 99 and es-api-import.ind-situacao   <> cb-estado      then NEXT.

            FIND FIRST es-api-param WHERE es-api-param.ind-tipo-trans = es-api-import.ind-tipo-trans
                                      AND es-api-param.cd-tipo-integr = es-api-import.cd-tipo-integr NO-LOCK NO-ERROR.
            IF NOT AVAIL es-api-param THEN NEXT.

            IF cb-sistema     <> 99 AND es-api-param.cd-sistema     <> cb-sistema     then NEXT.

            CREATE tt-movto.
            ASSIGN tt-movto.transacao       = IF es-api-import.ind-tipo-trans = 1 THEN "Importa‡Æo" ELSE "Exporta‡Æo"
                   tt-movto.id-movto        = es-api-import.id-movto
                   tt-movto.cd-tipo         = es-api-import.cd-tipo-integr
                   tt-movto.des-tipo-integr = es-api-param.des-tipo-integr
                   tt-movto.cd-tipo-integr  = es-api-param.cd-tipo-integr
                   tt-movto.chave           = es-api-import.chave
                   tt-movto.dt-inicio       = es-api-import.data-inicio
                   tt-movto.dt-fim          = es-api-import.data-fim
                   tt-movto.ind-tipo-trans  = es-api-param.ind-tipo-trans.

            CASE es-api-import.cod-status:
                WHEN 0 THEN tt-movto.cd-status = "NÆo Verificado".
                WHEN 1 THEN tt-movto.cd-status = "Integrado (OK)".
                WHEN 2 THEN tt-movto.cd-status = "NÆo Integrado (Erro)".
            END CASE.
            
            FOR EACH es-api-import-log OF es-api-import NO-LOCK:
                CREATE tt-log.
                ASSIGN tt-log.id-movto = es-api-import.id-movto
                       tt-log.data     = es-api-import-log.data
                       tt-log.cd-tipo  = es-api-import.cd-tipo-integr
                       tt-log.nr-seq   = es-api-import-log.nr-seq
                       tt-log.c-log    = trim(es-api-import-log.des-log). 
            END.
        END.        
    END.
    IF rs-transacao = 1 OR rs-transacao = 3 THEN DO:

        FOR EACH es-api-export NO-LOCK 
           WHERE DATE(es-api-export.data-movto) = DATE(fi-dt-movto:SCREEN-VALUE IN FRAME {&FRAME-NAME})                                      /*AND es-api-export.chave BEGINS fi-chave*/ :

            IF cb-tipo-integr <> 99 AND es-api-export.cd-tipo-integr <> cb-tipo-integr then NEXT.
            IF cb-status      <> 99 and es-api-export.cod-status     <> cb-status      then NEXT.
            IF cb-estado      <> 99 and es-api-export.ind-situacao   <> cb-estado      then NEXT.
            

            FIND FIRST es-api-param WHERE es-api-param.ind-tipo-trans = es-api-export.ind-tipo-trans
                                      AND es-api-param.cd-tipo-integr = es-api-export.cd-tipo-integr NO-LOCK NO-ERROR.

            IF NOT AVAIL es-api-param THEN NEXT.

            IF cb-sistema     <> 99 AND es-api-param.cd-sistema     <> cb-sistema     then NEXT.

            CREATE tt-movto.
            ASSIGN tt-movto.transacao       = IF es-api-export.ind-tipo-trans = 1 THEN "Importa‡Æo" ELSE "Exporta‡Æo"
                   tt-movto.id-movto        = es-api-export.id-movto
                   tt-movto.cd-tipo         = es-api-export.cd-tipo-integr
                   tt-movto.des-tipo-integr = es-api-param.des-tipo-integr
                   tt-movto.cd-tipo-integr  = es-api-param.cd-tipo-integr
                   tt-movto.chave           = es-api-export.chave
                   tt-movto.dt-inicio       = es-api-export.data-inicio
                   tt-movto.dt-fim          = es-api-export.data-fim
                   tt-movto.ind-tipo-trans  = es-api-param.ind-tipo-trans.

            CASE es-api-export.cod-status:
                WHEN 0 THEN tt-movto.cd-status = "NÆo Verificado".
                WHEN 1 THEN tt-movto.cd-status = "Integrado (OK)".
                WHEN 2 THEN tt-movto.cd-status = "NÆo Integrado (Erro)".
            END CASE.

            FOR EACH es-api-export-log OF es-api-export NO-LOCK:
                CREATE tt-log.
                ASSIGN tt-log.id-movto = es-api-export.id-movto
                       tt-log.cd-tipo  = es-api-export.cd-tipo-integr
                       tt-log.data     = es-api-export-log.data
                       tt-log.nr-seq   = es-api-export-log.nr-seq
                       tt-log.c-log    = es-api-export-log.des-log.
            END.
        END.
    END.


    OPEN QUERY br-movto
        FOR EACH tt-movto BY tt-movto.dt-fim DESC BY tt-movto.id-movto.

    APPLY 'value-changed' TO br-movto.

    ASSIGN fi-chave:SCREEN-VALUE = "".

    SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-rep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-rep w-livre
ON CHOOSE OF bt-rep IN FRAME f-cad /* Reprocessar */
DO:    
    IF AVAIL tt-movto THEN do: 

        IF tt-movto.transacao = "Exporta‡Æo" THEN DO: 

            IF tt-movto.cd-status <> "Integrado (OK)" THEN DO:
            
                FIND FIRST es-api-export EXCLUSIVE-LOCK WHERE es-api-export.id-movto = tt-movto.id-movto NO-ERROR.
                IF AVAIL es-api-export THEN DO:
        
                    ASSIGN es-api-export.cod-status   = 0
                           es-api-export.ind-situacao = 1.
        
                    RUN pi-processa (INPUT es-api-export.ind-tipo-trans, INPUT es-api-export.cd-tipo-integr).
        
                    FOR EACH es-api-export-log OF es-api-export NO-LOCK:
        
                        IF NOT CAN-FIND(FIRST tt-log WHERE tt-log.id-movto = int(es-api-export.id-movto       )
                                                       AND tt-log.cd-tipo  = es-api-export.cd-tipo-integr 
                                                       AND tt-log.nr-seq   = es-api-export-log.nr-seq     ) THEN DO:
                            CREATE tt-log.
                            ASSIGN tt-log.id-movto = es-api-export.id-movto
                                   tt-log.cd-tipo  = es-api-export.cd-tipo-integr
                                   tt-log.nr-seq   = es-api-export-log.nr-seq
                                   tt-log.c-log    = es-api-export-log.des-log.
                        END.
                    END.
        
                    OPEN QUERY br-log
                        FOR EACH tt-log OF tt-movto BY tt-log.nr-seq DESC.
        
                    APPLY "row-diplay" TO br-movto.
                    br-movto:REFRESH().
        
                    RELEASE es-api-export.
                    
                END.
            END.
        END.
        ELSE DO:

            IF tt-movto.cd-status <> "Integrado (OK)" THEN DO:

                FIND FIRST es-api-import EXCLUSIVE-LOCK WHERE es-api-import.id-movto = tt-movto.id-movto NO-ERROR.
                IF AVAIL es-api-import THEN DO:

                    ASSIGN es-api-import.cod-status   = 0
                           es-api-import.ind-situacao = 1.
        
                    RUN pi-processa (INPUT es-api-import.ind-tipo-trans, INPUT es-api-import.cd-tipo-integr).
        
                    FOR EACH es-api-import-log OF es-api-import NO-LOCK:
        
                        IF NOT CAN-FIND(FIRST tt-log WHERE tt-log.id-movto = int(es-api-import.id-movto       )
                                                       AND tt-log.cd-tipo  = es-api-import.cd-tipo-integr 
                                                       AND tt-log.nr-seq   = es-api-import-log.nr-seq     ) THEN DO:
                            CREATE tt-log.
                            ASSIGN tt-log.id-movto = es-api-import.id-movto
                                   tt-log.cd-tipo  = es-api-import.cd-tipo-integr
                                   tt-log.nr-seq   = es-api-import-log.nr-seq
                                   tt-log.c-log    = es-api-import-log.des-log.
                        END.
                    END.
        
                    OPEN QUERY br-log
                        FOR EACH tt-log OF tt-movto BY tt-log.nr-seq DESC.
        
                    APPLY "row-diplay" TO br-movto.
                    br-movto:REFRESH().
        
                    RELEASE es-api-import.
                    
                END.
            END.


        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-sistema
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-sistema w-livre
ON VALUE-CHANGED OF cb-sistema IN FRAME f-cad /* Sistema */
DO:

    DEF VAR c-integr  AS CHARACTER NO-UNDO.
    DEF VAR l-ret     AS LOGICAL   NO-UNDO.

    ASSIGN FRAME f-cad rs-transacao.

    ASSIGN 
       c-integr  = "Todas,99".

    FIND FIRST es-api-app NO-LOCK
         WHERE es-api-app.cd-sistema = SELF:INPUT-VALUE
         NO-ERROR.

    CASE rs-transacao:
        WHEN 1 THEN DO:
            /*DISABLE cb-tipo-integr WITH FRAME f-cad.           */
        END.
        WHEN 2 THEN DO:
            ENABLE cb-tipo-integr WITH FRAME f-cad. 
            FOR EACH es-api-param WHERE es-api-param.ind-tipo-trans = 1 NO-LOCK:
                IF es-api-param.cd-sistema = SELF:INPUT-VALUE
                OR SELF:INPUT-VALUE = 99
                THEN ASSIGN 
                   c-integr = c-integr + "," + es-api-param.des-tipo-integr + "," + string(es-api-param.cd-tipo-integr).
            END.
        END.
        WHEN 3 THEN DO:
            ENABLE cb-tipo-integr WITH FRAME f-cad. 
            FOR EACH es-api-param WHERE es-api-param.ind-tipo-trans = 2 NO-LOCK:
                IF es-api-param.cd-sistema = SELF:INPUT-VALUE
                OR SELF:INPUT-VALUE = 99
                THEN ASSIGN 
                   c-integr = c-integr + "," + es-api-param.des-tipo-integr + "," + string(es-api-param.cd-tipo-integr) .
            END.
        END.

    END CASE.

    ASSIGN 
       cb-tipo-integr:LIST-ITEM-PAIRS = c-integr
       cb-tipo-integr:SCREEN-VALUE = "99".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-chave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-chave w-livre
ON RETURN OF fi-chave IN FRAME f-cad /* Chave */
DO:
    APPLY "choose" TO bt-pesquisar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-livre
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-livre
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-livre
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-programa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-programa w-livre
ON MENU-DROP OF MENU mi-programa /* Nome-do-Programa */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-livre
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-livre
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Copia_Chave_para_rea_de_tra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Copia_Chave_para_rea_de_tra w-livre
ON CHOOSE OF MENU-ITEM m_Copia_Chave_para_rea_de_tra /* Copia Chave para  rea de transferˆncia */
DO:
    IF AVAIL tt-movto THEN
        CLIPBOARD:VALUE = tt-movto.chave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Visualiza_Json
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Visualiza_Json w-livre
ON CHOOSE OF MENU-ITEM m_Visualiza_Json /* Visualiza Json */
DO:

    DEF VAR cLongJson AS LONGCHAR  NO-UNDO.
    DEF VAR h-buffer  AS HANDLE    NO-UNDO.
    DEF VAR h-query   AS HANDLE    NO-UNDO.
    DEF VAR i-count   AS INTEGER   NO-UNDO.
    
    IF AVAIL tt-movto THEN DO:

        FIND FIRST es-api-param NO-LOCK WHERE es-api-param.ind-tipo-trans = tt-movto.ind-tipo-trans 
                                          AND es-api-param.cd-tipo-integr = tt-movto.cd-tipo-integr NO-ERROR.

        IF AVAIL es-api-param THEN DO:

            CREATE BUFFER h-buffer FOR TABLE es-api-param.nome-tabela-integr.
                                                                  
            CREATE QUERY h-query.
            h-query:SET-BUFFERS(h-buffer).
            h-query:QUERY-PREPARE("FOR EACH " + es-api-param.nome-tabela-integr + " where id-movto = " + STRING(tt-movto.id-movto)).
            h-query:QUERY-OPEN().
            
            REPEAT:
                h-query:GET-NEXT().  
                if h-query:QUERY-OFF-END THEN LEAVE.

                COPY-LOB h-buffer:BUFFER-FIELD("c-json"):BUFFER-VALUE() TO cLongJson.                 
            END.
            
            h-query:QUERY-CLOSE().
            h-buffer:BUFFER-RELEASE().
            DELETE OBJECT h-buffer.
            DELETE OBJECT h-query.

            IF clongjson = "" THEN
                MESSAGE 'sem registros' VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RUN esp\esint006j (INPUT es-api-param.ind-tipo-trans,
                               INPUT clongjson).

        END.        
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Visualiza_Retorno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Visualiza_Retorno w-livre
ON CHOOSE OF MENU-ITEM m_Visualiza_Retorno /* Visualiza Retorno */
DO:
    DEF VAR cLongJson AS LONGCHAR  NO-UNDO.
    DEF VAR h-buffer  AS HANDLE    NO-UNDO.
    DEF VAR h-query   AS HANDLE    NO-UNDO.
    DEF VAR i-count   AS INTEGER   NO-UNDO.
    
    IF AVAIL tt-movto THEN DO:
        FIND FIRST es-api-param NO-LOCK WHERE es-api-param.ind-tipo-trans = tt-movto.ind-tipo-trans 
                                          AND es-api-param.cd-tipo-integr = tt-movto.cd-tipo-integr  NO-ERROR.
        IF AVAIL es-api-param THEN DO:
            FIND FIRST es-api-export NO-LOCK WHERE es-api-export.id-movto = tt-movto.id-movto NO-ERROR.
            IF AVAIL es-api-export THEN DO:
                ASSIGN clongjson = es-api-export.text-retorno.
                RUN esp\esint006j (INPUT clongjson).
          END.
       END.        
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-transacao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-transacao w-livre
ON VALUE-CHANGED OF rs-transacao IN FRAME f-cad
DO:
    DEF VAR c-sistema AS CHARACTER NO-UNDO.
    DEF VAR l-ret     AS LOGICAL   NO-UNDO.

    ASSIGN FRAME f-cad rs-transacao.

    ASSIGN 
       c-sistema = "Todos,99".

    FOR EACH es-api-app NO-LOCK:
        ASSIGN 
           c-sistema = c-sistema
                     + "," 
                     + es-api-app.des-sistema
                     + "," 
                     + string(es-api-app.cd-sistema).
    END.

    
    ASSIGN 
       cb-sistema    :LIST-ITEM-PAIRS = c-sistema
       cb-sistema    :SCREEN-VALUE = "99".
    APPLY "VALUE-CHANGED" TO cb-sistema IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-log
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-livre  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.13 , 95.72 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-livre  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-livre  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
  THEN DELETE WIDGET w-livre.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-livre  _DEFAULT-ENABLE
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
  DISPLAY cb-sistema cb-tipo-integr rs-transacao cb-estado cb-status fi-dt-movto 
          fi-chave 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE bt-rep rt-button RECT-4 RECT-5 cb-sistema cb-tipo-integr rs-transacao 
         bt-pesquisar cb-estado bt-sla cb-status fi-dt-movto bt-rel fi-chave 
         bt-agend bt-faixa br-movto br-log 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-livre 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  run pi-before-initialize.

  {include/win-size.i}

  {utp/ut9000.i "esint006" "1.00.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  APPLY 'value-changed' TO rs-transacao.

  ASSIGN fi-dt-movto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY,"99/99/9999").

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-livre  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-movto"}
  {src/adm/template/snd-list.i "tt-log"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-livre 
PROCEDURE state-changed :
/*:T -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


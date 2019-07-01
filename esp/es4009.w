&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgcam            PROGRESS
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
**
*******************************************************************************/

{include/i-prgvrs.i ES4009 12.1.20.001}

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> <m¢dulo>}
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

DEFINE NEW GLOBAL SHARED VARIABLE gr-item  AS ROWID     NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE rRowid   AS ROWID     NO-UNDO.

FIND ITEM NO-LOCK WHERE ROWID(item) = gr-item NO-ERROR.
IF NOT AVAIL ITEM THEN RETURN 'NOK':U.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es-item.unidade-saco es-item.tem-cdo ~
es-item.tem-funrural es-item.u-int-1 es-item.u-dec-1 ~
es-item.receita-certif-analise es-item.embalagem-laudo-pesol ~
es-item.curva-esterilizacao-pad 
&Scoped-define ENABLED-TABLES es-item
&Scoped-define FIRST-ENABLED-TABLE es-item
&Scoped-Define ENABLED-OBJECTS fi-categoria-item bt-ok bt-cancela bt-ajuda ~
rt-buttom 
&Scoped-Define DISPLAYED-FIELDS es-item.unidade-saco es-item.tem-cdo ~
es-item.tem-funrural es-item.u-int-1 es-item.u-dec-1 ~
es-item.receita-certif-analise es-item.embalagem-laudo-pesol ~
es-item.curva-esterilizacao-pad 
&Scoped-define DISPLAYED-TABLES es-item
&Scoped-define FIRST-DISPLAYED-TABLE es-item
&Scoped-Define DISPLAYED-OBJECTS fi-categoria-item fi-desc-categoria 

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


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-categoria-item AS CHARACTER FORMAT "X(256)":U 
     LABEL "Categoria Item" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-categoria AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45.86 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 102 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     es-item.unidade-saco AT ROW 1.25 COL 40.72 COLON-ALIGNED WIDGET-ID 196
          LABEL "Unidade Saco para Contrato"
          VIEW-AS FILL-IN 
          SIZE 13.72 BY .88
     es-item.tem-cdo AT ROW 2.21 COL 40.72 COLON-ALIGNED WIDGET-ID 188
          LABEL "Tem Cdo"
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     es-item.tem-funrural AT ROW 3.17 COL 40.72 COLON-ALIGNED WIDGET-ID 190
          LABEL "Tem Funrural"
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     es-item.u-int-1 AT ROW 4.13 COL 40.72 COLON-ALIGNED WIDGET-ID 194
          LABEL "Codigo do incentivo"
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     es-item.u-dec-1 AT ROW 5.04 COL 40.72 COLON-ALIGNED WIDGET-ID 192
          LABEL "Quantidade na embalagem"
          VIEW-AS FILL-IN 
          SIZE 14 BY .88
     es-item.receita-certif-analise AT ROW 6 COL 40.72 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 60 BY .88
     es-item.embalagem-laudo-pesol AT ROW 6.96 COL 40.72 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 60 BY .88
     es-item.curva-esterilizacao-pad AT ROW 7.92 COL 40.72 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 60 BY .88
     fi-categoria-item AT ROW 8.92 COL 40.72 COLON-ALIGNED WIDGET-ID 8
     fi-desc-categoria AT ROW 8.92 COL 54.86 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     bt-ok AT ROW 10.29 COL 3 WIDGET-ID 184
     bt-cancela AT ROW 10.29 COL 14 WIDGET-ID 182
     bt-ajuda AT ROW 10.29 COL 92.86 WIDGET-ID 180
     rt-buttom AT ROW 10.08 COL 2 WIDGET-ID 186
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 103.14 BY 10.71 WIDGET-ID 100.


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
         TITLE              = "ES4009"
         HEIGHT             = 10.54
         WIDTH              = 103.14
         MAX-HEIGHT         = 27.5
         MAX-WIDTH          = 195.14
         VIRTUAL-HEIGHT     = 27.5
         VIRTUAL-WIDTH      = 195.14
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
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN fi-desc-categoria IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN es-item.tem-cdo IN FRAME f-cad
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-item.tem-funrural IN FRAME f-cad
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-item.u-dec-1 IN FRAME f-cad
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-item.u-int-1 IN FRAME f-cad
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-item.unidade-saco IN FRAME f-cad
   EXP-LABEL                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* ES4009 */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* ES4009 */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */

    DEF VAR l-erro AS LOG NO-UNDO.

    RUN pi-valida (OUTPUT l-erro).

    IF l-erro = NO THEN DO:

        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
        
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-livre
ON CHOOSE OF bt-ajuda IN FRAME f-cad /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancela w-livre
ON CHOOSE OF bt-cancela IN FRAME f-cad /* Cancelar */
DO:

    DEF VAR l-erro AS LOG NO-UNDO.

    RUN pi-valida (OUTPUT l-erro).

    IF l-erro = NO THEN DO:

        APPLY "CLOSE":U TO THIS-PROCEDURE.
        
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-livre
ON CHOOSE OF bt-ok IN FRAME f-cad /* OK */
DO:

    DEF VAR l-erro AS LOG NO-UNDO.
    

    RUN pi-valida (OUTPUT l-erro).

    IF l-erro = NO THEN DO:

        FIND FIRST es-it-categoria WHERE
                   es-it-categoria.it-codigo = ITEM.it-codigo EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAIL es-it-categoria THEN DO:
    
            CREATE es-it-categoria.
            ASSIGN es-it-categoria.it-codigo     = ITEM.it-codigo
                   es-it-categoria.cod-categoria = fi-categoria-item:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
    
        END.
        ELSE
            ASSIGN es-it-categoria.cod-categoria = fi-categoria-item:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
    
                   
        
        FIND es-item
            WHERE es-item.it-codigo = ITEM.it-codigo 
            NO-ERROR.
    
    
        IF NOT AVAIL es-item THEN DO:
           CREATE es-item.
           ASSIGN es-item.it-codigo  = ITEM.it-codigo.
        END.
    
        ASSIGN es-item.unidade-saco = INPUT es-item.unidade-saco
               es-item.tem-cdo      = INPUT es-item.tem-cdo
               es-item.tem-funrural = INPUT es-item.tem-funrural
    /*            es-item.u-char-1     = INPUT es-item.u-char-1  */ /* 31/05/2006 */
               es-item.u-int-1      = INPUT es-item.u-int-1 
               es-item.u-dec-1      = INPUT es-item.u-dec-1
               es-item.receita-certif-analise  = INPUT es-item.receita-certif-analise   /* rde - 12/04/2012 */
               es-item.embalagem-laudo-pesol   = INPUT es-item.embalagem-laudo-pesol    /* rde - 16/04/2012 */
               es-item.curva-esterilizacao-pad = INPUT es-item.curva-esterilizacao-pad. /* rde - 17/04/2012 */

        APPLY "CLOSE":U TO THIS-PROCEDURE.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-categoria-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-categoria-item w-livre
ON LEAVE OF fi-categoria-item IN FRAME f-cad /* Categoria Item */
DO:

    FIND FIRST es-categoria-item WHERE
               es-categoria-item.cod-categoria = fi-categoria-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
    IF AVAIL es-categoria-item THEN
        ASSIGN fi-desc-categoria:SCREEN-VALUE IN FRAME {&FRAME-NAME} = es-categoria-item.desc-categoria.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-categoria-item w-livre
ON MOUSE-SELECT-DBLCLICK OF fi-categoria-item IN FRAME f-cad /* Categoria Item */
DO:
  APPLY 'F5'.

DEF VAR c-cod-categoria AS CHAR NO-UNDO.
   
RUN esp/es4009-z01.w (OUTPUT c-cod-categoria).

ASSIGN fi-categoria-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-cod-categoria.


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


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-livre
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  DISPLAY fi-categoria-item fi-desc-categoria 
      WITH FRAME f-cad IN WINDOW w-livre.
  IF AVAILABLE es-item THEN 
    DISPLAY es-item.unidade-saco es-item.tem-cdo es-item.tem-funrural 
          es-item.u-int-1 es-item.u-dec-1 es-item.receita-certif-analise 
          es-item.embalagem-laudo-pesol es-item.curva-esterilizacao-pad 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE es-item.unidade-saco es-item.tem-cdo es-item.tem-funrural 
         es-item.u-int-1 es-item.u-dec-1 es-item.receita-certif-analise 
         es-item.embalagem-laudo-pesol es-item.curva-esterilizacao-pad 
         fi-categoria-item bt-ok bt-cancela bt-ajuda rt-buttom 
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

  {utp/ut9000.i "MOPD1270" "12.1.20.001"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  fi-categoria-item:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.

  RUN pi-010-inicializar.

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-010-inicializar w-livre 
PROCEDURE pi-010-inicializar :
/*------------------------------------------------------------------------------
  Notes:       
------------------------------------------------------------------------------*/

FIND es-item
    WHERE es-item.it-codigo = ITEM.it-codigo
    NO-ERROR.

/* 02/06/2006 */

/* IF NOT AVAIL es-item THEN DO:                   */
/*    CREATE es-item.                              */
/*    ASSIGN es-item.it-codigo  = ITEM.it-codigo.  */
/* END.                                            */

IF NOT AVAIL es-item THEN DO:
   CREATE es-item.
   ASSIGN es-item.it-codigo  = ITEM.it-codigo.
END.
ELSE DO:
   ASSIGN es-item.cod-ean    = ITEM.inform-compl.
END.

ASSIGN es-item.unidade-saco           :SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(es-item.unidade-saco)
       es-item.tem-cdo                :SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(es-item.tem-cdo)
       es-item.tem-funrural           :SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(es-item.tem-funrural)
       es-item.u-int-1                :SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(es-item.u-int-1)
       es-item.u-dec-1                :SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(es-item.u-dec-1)
       es-item.receita-certif-analise :SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(es-item.receita-certif-analise)   /* rde - 12/04/2012 */
       es-item.embalagem-laudo-pesol  :SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(es-item.embalagem-laudo-pesol)    /* rde - 16/04/2012 */
       es-item.curva-esterilizacao-pad:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(es-item.curva-esterilizacao-pad). /* rde - 17/04/2012 */

FIND FIRST es-it-categoria WHERE
           es-it-categoria.it-codigo = ITEM.it-codigo NO-LOCK NO-ERROR.
IF AVAIL es-it-categoria THEN
    ASSIGN fi-categoria-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = es-it-categoria.cod-categoria.

FIND FIRST es-categoria WHERE
           es-categoria.cod-categoria = es-it-categoria.cod-categoria NO-LOCK NO-ERROR.
IF AVAIL es-categoria THEN
    ASSIGN fi-desc-categoria:SCREEN-VALUE IN FRAME {&FRAME-NAME} = es-categoria.desc-categoria.

       

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-valida w-livre 
PROCEDURE pi-valida :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF OUTPUT PARAMETER p-erro   AS LOG NO-UNDO. 

IF fi-categoria-item:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = "" THEN DO:

    RUN utp/ut-msgs.p (INPUT "SHOW":U,
                       INPUT 17006,
                       INPUT 'Categoria Item ~~ Obrigat¢rio preencher Categoria do Item.'). 

    ASSIGN p-erro = YES.

END.
ELSE DO:

    FIND FIRST es-categoria-item WHERE
               es-categoria-item.cod-categoria = fi-categoria-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
    IF NOT AVAIL es-categoria-item THEN DO:

        RUN utp/ut-msgs.p (INPUT "SHOW":U,
                           INPUT 17006,
                           INPUT 'Categoria Item ~~ Categoria do Item n∆o CADASTRADA!.'). 
    
        ASSIGN p-erro = YES.

    END.


END.

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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-livre, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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


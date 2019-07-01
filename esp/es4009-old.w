&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgcam            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
**
** 02/06/2006 RNK - Foi excluido o campo C¢digo EAN da tela e a atualizaá∆o
**                  desse campo passou a ser dentro desse programa.
** 09/12/2008 RNK - Foi incluido o campo Quantidade na embalagem (foi utilizado 
**                  o campo es-item.u-dec-1).
*******************************************************************************/
{include/i-prgvrs.i ES4009 2.09.00.001 } /*** 010101 ***/

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES es-item

/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define QUERY-STRING-D-Dialog FOR EACH es-item SHARE-LOCK
&Scoped-define OPEN-QUERY-D-Dialog OPEN QUERY D-Dialog FOR EACH es-item SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-D-Dialog es-item
&Scoped-define FIRST-TABLE-IN-QUERY-D-Dialog es-item


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es-item.unidade-saco es-item.tem-cdo ~
es-item.tem-funrural es-item.u-int-1 es-item.u-dec-1 ~
es-item.receita-certif-analise es-item.embalagem-laudo-pesol ~
es-item.curva-esterilizacao-pad 
&Scoped-define ENABLED-TABLES es-item
&Scoped-define FIRST-ENABLED-TABLE es-item
&Scoped-Define ENABLED-OBJECTS RECT-1 rt-buttom fi-categoria-item bt-ok ~
bt-cancela bt-okk bt-ajuda 
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

/* Define a dialog box                                                  */

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

DEFINE BUTTON bt-okk 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE fi-categoria-item AS CHARACTER FORMAT "X(256)":U 
     LABEL "Categoria Item" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-categoria AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45.86 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 103 BY 9.25.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 102 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY D-Dialog FOR 
      es-item SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     es-item.unidade-saco AT ROW 1.25 COL 40.72 COLON-ALIGNED
          LABEL "Unidade Saco para Contrato"
          VIEW-AS FILL-IN 
          SIZE 13.72 BY .88
     es-item.tem-cdo AT ROW 2.21 COL 40.72 COLON-ALIGNED
          LABEL "Tem Cdo"
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     es-item.tem-funrural AT ROW 3.17 COL 40.72 COLON-ALIGNED
          LABEL "Tem Funrural"
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     es-item.u-int-1 AT ROW 4.13 COL 40.72 COLON-ALIGNED
          LABEL "Codigo do incentivo"
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     es-item.u-dec-1 AT ROW 5.04 COL 40.72 COLON-ALIGNED
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
     bt-ok AT ROW 10.63 COL 3
     bt-cancela AT ROW 10.63 COL 14
     bt-okk AT ROW 10.63 COL 28 WIDGET-ID 12
     bt-ajuda AT ROW 10.63 COL 92.86
     RECT-1 AT ROW 1 COL 1
     rt-buttom AT ROW 10.38 COL 2
     SPACE(0.28) SKIP(0.36)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Extens∆o do Item"
         DEFAULT-BUTTON bt-ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/d-dialog.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME L-To-R                                                    */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-desc-categoria IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN es-item.tem-cdo IN FRAME D-Dialog
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-item.tem-funrural IN FRAME D-Dialog
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-item.u-dec-1 IN FRAME D-Dialog
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-item.u-int-1 IN FRAME D-Dialog
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-item.unidade-saco IN FRAME D-Dialog
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _TblList          = "es-item"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Extens∆o do Item */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda D-Dialog
ON CHOOSE OF bt-ajuda IN FRAME D-Dialog /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* OK */
DO:
    
    RUN pi-valida.

    FIND FIRST es-it-categoria WHERE
               es-it-categoria.it-codigo = ITEM.it-codigo NO-LOCK NO-ERROR.
    IF NOT AVAIL es-it-categoria THEN DO:

        CREATE es-it-categoria.
        ASSIGN es-it-categoria.cod-categoria = fi-categoria-item:SCREEN-VALUE IN FRAME {&FRAME-NAME}.

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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-okk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-okk D-Dialog
ON CHOOSE OF bt-okk IN FRAME D-Dialog /* OK */
DO:
    
    RUN pi-valida.

    MESSAGE "aqui"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    FIND FIRST es-it-categoria WHERE
               es-it-categoria.it-codigo = ITEM.it-codigo NO-LOCK NO-ERROR.
    IF NOT AVAIL es-it-categoria THEN DO:

        CREATE es-it-categoria.
        ASSIGN es-it-categoria.cod-categoria = fi-categoria-item:SCREEN-VALUE IN FRAME {&FRAME-NAME}.

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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-categoria-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-categoria-item D-Dialog
ON LEAVE OF fi-categoria-item IN FRAME D-Dialog /* Categoria Item */
DO:

    FIND FIRST es-categoria-item WHERE
               es-categoria-item.cod-categoria = fi-categoria-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
    IF AVAIL es-categoria-item THEN
        ASSIGN fi-desc-categoria:SCREEN-VALUE IN FRAME {&FRAME-NAME} = es-categoria-item.desc-categoria.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-categoria-item D-Dialog
ON MOUSE-SELECT-DBLCLICK OF fi-categoria-item IN FRAME D-Dialog /* Categoria Item */
DO:
  APPLY 'F5'.

DEF VAR c-cod-categoria AS CHAR NO-UNDO.
   
RUN esp/es4009-z01.w (OUTPUT c-cod-categoria).

ASSIGN fi-categoria-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-cod-categoria.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
      WITH FRAME D-Dialog.
  IF AVAILABLE es-item THEN 
    DISPLAY es-item.unidade-saco es-item.tem-cdo es-item.tem-funrural 
          es-item.u-int-1 es-item.u-dec-1 es-item.receita-certif-analise 
          es-item.embalagem-laudo-pesol es-item.curva-esterilizacao-pad 
      WITH FRAME D-Dialog.
  ENABLE RECT-1 rt-buttom es-item.unidade-saco es-item.tem-cdo 
         es-item.tem-funrural es-item.u-int-1 es-item.u-dec-1 
         es-item.receita-certif-analise es-item.embalagem-laudo-pesol 
         es-item.curva-esterilizacao-pad fi-categoria-item bt-ok bt-cancela 
         bt-okk bt-ajuda 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy D-Dialog 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  {utp/ut9000.i "ES4009" "2.04.01.001"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  fi-categoria-item:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-valida D-Dialog 
PROCEDURE pi-valida :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF fi-categoria-item:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = "" THEN DO:

    RUN utp/ut-msgs.p (INPUT "SHOW":U,
                       INPUT 17006,
                       INPUT 'Categoria Item ~~ Obrigat¢rio preencher Categoria do Item.'). 

    RETURN 'ADM-ERROR':U.

END.
ELSE DO:

    FIND FIRST es-categoria-item WHERE
               es-categoria-item.cod-categoria = fi-categoria-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
    IF NOT AVAIL es-categoria-item THEN DO:

        RUN utp/ut-msgs.p (INPUT "SHOW":U,
                           INPUT 17006,
                           INPUT 'Categoria Item ~~ Categoria do Item n∆o CADASTRADA!.'). 
    
        RETURN 'ADM-ERROR':U.

    END.


END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "es-item"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
  
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


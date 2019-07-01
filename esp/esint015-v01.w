&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgcam            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i V99XX999 9.99.99.999}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m¢dulo dever  ser MUT                    */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> MUT}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w

/* global variable definitions */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES es-api-schedule
&Scoped-define FIRST-EXTERNAL-TABLE es-api-schedule


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es-api-schedule.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es-api-schedule.cd-tipo-integr ~
es-api-schedule.data-inicio es-api-schedule.data-fim es-api-schedule.ativo 
&Scoped-define ENABLED-TABLES es-api-schedule
&Scoped-define FIRST-ENABLED-TABLE es-api-schedule
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS es-api-schedule.cd-tipo-integr ~
es-api-schedule.data-inicio es-api-schedule.data-fim es-api-schedule.ativo 
&Scoped-define DISPLAYED-TABLES es-api-schedule
&Scoped-define FIRST-DISPLAYED-TABLE es-api-schedule
&Scoped-Define DISPLAYED-OBJECTS c-des-tipo-integr i-cd-sistema ~
c-des-sistema c-hora-inicio c-hora-fim c-intervalo 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS c-hora-inicio c-hora-fim c-intervalo 
&Scoped-define ADM-ASSIGN-FIELDS c-hora-inicio c-hora-fim c-intervalo 
&Scoped-define ADM-MODIFY-FIELDS c-hora-inicio c-hora-fim c-intervalo 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
cd-tipo-integr||y|mgcam.es-api-schedule.cd-tipo-integr
id-agenda|y|y|mgcam.es-api-schedule.id-agenda
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "id-agenda",
     Keys-Supplied = "cd-tipo-integr,id-agenda"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fHora V-table-Win 
FUNCTION fHora RETURNS INTEGER
  ( t AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE c-des-sistema AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE c-des-tipo-integr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE c-hora-fim AS CHARACTER FORMAT "99:99:99":U INITIAL "235959" 
     LABEL "Hora Final" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY 1 NO-UNDO.

DEFINE VARIABLE c-hora-inicio AS CHARACTER FORMAT "99:99:99":U INITIAL "000000" 
     LABEL "Hora Inicial" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY 1 NO-UNDO.

DEFINE VARIABLE c-intervalo AS CHARACTER FORMAT "99:99:99":U INITIAL "000000" 
     LABEL "Intevalo" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY 1 NO-UNDO.

DEFINE VARIABLE i-cd-sistema AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Sistema" 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY 1 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 2.75.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     es-api-schedule.cd-tipo-integr AT ROW 1.25 COL 17 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 11.43 BY 1
     c-des-tipo-integr AT ROW 1.25 COL 29 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     i-cd-sistema AT ROW 2.5 COL 17 COLON-ALIGNED WIDGET-ID 18
     c-des-sistema AT ROW 2.5 COL 29 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     es-api-schedule.data-inicio AT ROW 4.04 COL 17 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 12.57 BY 1
     es-api-schedule.data-fim AT ROW 4.04 COL 59 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 12.57 BY 1
     c-hora-inicio AT ROW 5.21 COL 17 COLON-ALIGNED WIDGET-ID 22
     c-hora-fim AT ROW 5.21 COL 59 COLON-ALIGNED WIDGET-ID 24
     c-intervalo AT ROW 6.33 COL 17 COLON-ALIGNED WIDGET-ID 26
     es-api-schedule.ativo AT ROW 7.46 COL 19 WIDGET-ID 20
          VIEW-AS TOGGLE-BOX
          SIZE 11.57 BY .83
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 3.75 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgcam.es-api-schedule
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 7.83
         WIDTH              = 88.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}
{include/i_dbtype.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN c-des-sistema IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-des-tipo-integr IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-hora-fim IN FRAME f-main
   NO-ENABLE 1 2 3                                                      */
/* SETTINGS FOR FILL-IN c-hora-inicio IN FRAME f-main
   NO-ENABLE 1 2 3                                                      */
/* SETTINGS FOR FILL-IN c-intervalo IN FRAME f-main
   NO-ENABLE 1 2 3                                                      */
/* SETTINGS FOR FILL-IN i-cd-sistema IN FRAME f-main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME es-api-schedule.cd-tipo-integr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-schedule.cd-tipo-integr V-table-Win
ON F5 OF es-api-schedule.cd-tipo-integr IN FRAME f-main /* Tipo de Integra‡Æo */
DO:
    {include/zoomvar.i 
        &prog-zoom="esp/esint004-z01.w"
        &campo=es-api-schedule.cd-tipo-integr
        &campozoom=cd-tipo-integr}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-schedule.cd-tipo-integr V-table-Win
ON LEAVE OF es-api-schedule.cd-tipo-integr IN FRAME f-main /* Tipo de Integra‡Æo */
DO:
   RUN pi-mostrar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-schedule.cd-tipo-integr V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-api-schedule.cd-tipo-integr IN FRAME f-main /* Tipo de Integra‡Æo */
DO:
   APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-api-schedule.data-fim
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

es-api-schedule.cd-tipo-integr:LOAD-MOUSE-POINTER("image/lupa.cur":U) IN FRAME {&FRAME-NAME}.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR key-value AS CHAR NO-UNDO.
  DEF VAR row-avail-enabled AS LOGICAL NO-UNDO.

  /* LOCK status on the find depends on FIELDS-ENABLED. */
  RUN get-attribute ('FIELDS-ENABLED':U).
  row-avail-enabled = (RETURN-VALUE eq 'yes':U).
  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'id-agenda':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = es-api-schedule
           &WHERE = "WHERE es-api-schedule.id-agenda eq INTEGER(key-value)"
       }
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "es-api-schedule"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es-api-schedule"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME f-main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    {include/i-valid.i}
    
    FIND FIRST es-api-param NO-LOCK
         WHERE es-api-param.cd-tipo-integr = es-api-schedule.cd-tipo-integr:INPUT-VALUE IN FRAME {&FRAME-NAME}
         NO-ERROR.
    IF NOT AVAIL es-api-param
    THEN DO:
       RUN utp/ut-msgs.p ("show",17006,"Tipo de integra‡Æo nÆo encontrada").
       APPLY "ENTRY" TO es-api-schedule.cd-tipo-integr IN FRAME {&FRAME-NAME}.
       RETURN 'ADM-ERROR':U.
    END.

    IF es-api-schedule.data-inicio:INPUT-VALUE IN FRAME {&FRAME-NAME} = ? 
    THEN DO:
       RUN utp/ut-msgs.p ("show",17006,"Data nÆo preenchida").
       APPLY "ENTRY" TO es-api-schedule.data-inicio IN FRAME {&FRAME-NAME}.
       RETURN 'ADM-ERROR':U.
    END.

    IF es-api-schedule.data-fim   :INPUT-VALUE IN FRAME {&FRAME-NAME} = ?
    THEN DO:
       RUN utp/ut-msgs.p ("show",17006,"Data nÆo preenchida").
       APPLY "ENTRY" TO es-api-schedule.data-fim IN FRAME {&FRAME-NAME}.
       RETURN 'ADM-ERROR':U.
    END.

    IF c-hora-inicio:INPUT-VALUE IN FRAME {&FRAME-NAME} = ""
    THEN DO:
       RUN utp/ut-msgs.p ("show",17006,"Hora nÆo preenchida").
       APPLY "ENTRY" TO c-hora-inicio IN FRAME {&FRAME-NAME}.
       RETURN 'ADM-ERROR':U.
    END.

    IF c-hora-fim   :INPUT-VALUE IN FRAME {&FRAME-NAME} = ""
    THEN DO:
       RUN utp/ut-msgs.p ("show",17006,"Hora nÆo preenchida").
       APPLY "ENTRY" TO c-hora-fim IN FRAME {&FRAME-NAME}.
       RETURN 'ADM-ERROR':U.
    END.

    IF fHora(c-hora-fim   :INPUT-VALUE IN FRAME {&FRAME-NAME}) > 86399    
    THEN DO:
       RUN utp/ut-msgs.p ("show",17006,"Hora inv lida").
       APPLY "ENTRY" TO c-hora-fim IN FRAME {&FRAME-NAME}.
       RETURN 'ADM-ERROR':U.
    END.

    IF c-intervalo        :INPUT-VALUE IN FRAME {&FRAME-NAME} = ""
    THEN DO:
       RUN utp/ut-msgs.p ("show",17006,"Hora nÆo preenchida").
       APPLY "ENTRY" TO c-hora-fim IN FRAME {&FRAME-NAME}.
       RETURN 'ADM-ERROR':U.
    END.


    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U 
    then return 'ADM-ERROR':U.
    
    ASSIGN
       es-api-schedule.hora-inicio = fHora(c-hora-inicio:INPUT-VALUE IN FRAME {&FRAME-NAME})
       es-api-schedule.hora-fim    = fHora(c-hora-fim   :INPUT-VALUE IN FRAME {&FRAME-NAME})
       es-api-schedule.intervalo   = fHora(c-intervalo  :INPUT-VALUE IN FRAME {&FRAME-NAME}).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  DEF BUFFER bf-es-api-schedule FOR es-api-schedule.
  
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  FIND LAST bf-es-api-schedule NO-LOCK
      WHERE bf-es-api-schedule.id-agenda > 0
      NO-ERROR.
  IF AVAIL bf-es-api-schedule
  THEN ASSIGN
     es-api-schedule.id-agenda = bf-es-api-schedule.id-agenda + 1.
  ELSE ASSIGN
     es-api-schedule.id-agenda = 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    disable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    
    DISABLE
       c-hora-inicio
       c-hora-fim
       c-intervalo
       WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
  RUN pi-mostrar.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    if adm-new-record = yes then
        enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    
    ENABLE
       c-hora-inicio
       c-hora-fim
       c-intervalo
       WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-parent V-table-Win 
PROCEDURE pi-atualiza-parent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define input parameter v-row-parent-externo as rowid no-undo.
    
    assign v-row-parent = v-row-parent-externo.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-mostrar V-table-Win 
PROCEDURE pi-mostrar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN
       i-cd-sistema     :SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
       c-des-sistema    :SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
       c-des-tipo-integr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "". 
    
    FIND FIRST es-api-param NO-LOCK
         WHERE es-api-param.cd-tipo-integr = es-api-schedule.cd-tipo-integr:INPUT-VALUE IN FRAME {&FRAME-NAME}
         NO-ERROR.
    IF AVAIL es-api-param
    THEN DO:
        ASSIGN
           c-des-tipo-integr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = es-api-param.des-tipo-integr.
        FIND FIRST es-api-app NO-LOCK
                OF es-api-param
             NO-ERROR.
        ASSIGN
           i-cd-sistema :SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(es-api-app.cd-sistema)
           c-des-sistema:SCREEN-VALUE IN FRAME {&FRAME-NAME} = es-api-app.des-sistema.
    END.
    IF AVAIL es-api-schedule
    THEN ASSIGN
       c-hora-inicio:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(es-api-schedule.hora-inicio,"hh:mm:ss")
       c-hora-fim   :SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(es-api-schedule.hora-fim   ,"hh:mm:ss")
       c-intervalo  :SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(es-api-schedule.intervalo  ,"hh:mm:ss").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*:T------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: NÆo fazer assign aqui. Nesta procedure
  devem ser colocadas apenas valida‡äes, pois neste ponto do programa o registro 
  ainda nÆo foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /*:T Valida‡Æo de dicion rio */
    
/*:T    Segue um exemplo de valida‡Æo de programa */
/*       find tabela where tabela.campo1 = c-variavel and               */
/*                         tabela.campo2 > i-variavel no-lock no-error. */
      
      /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
/*       {include/i-vldprg.i}                                             */
/*       run utp/ut-msgs.p (input "show":U, input 7, input return-value). */
/*       return 'ADM-ERROR':U.                                            */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "cd-tipo-integr" "es-api-schedule" "cd-tipo-integr"}
  {src/adm/template/sndkycas.i "id-agenda" "es-api-schedule" "id-agenda"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "es-api-schedule"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fHora V-table-Win 
FUNCTION fHora RETURNS INTEGER
  ( t AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN INT(SUBSTR(t,5,2))
       + INT(SUBSTR(t,3,2)) * 60
       + INT(SUBSTR(t,1,2)) * (60 * 60).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


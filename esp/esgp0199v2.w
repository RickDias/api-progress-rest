&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgcam            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESGP0199V2 2.09.00.001 } /*** 010001 ***/

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

{esp\esint001rp.i}

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
&Scoped-define EXTERNAL-TABLES es-gp-emit-cond-pagto
&Scoped-define FIRST-EXTERNAL-TABLE es-gp-emit-cond-pagto


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es-gp-emit-cond-pagto.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es-gp-emit-cond-pagto.cod-cond-pag 
&Scoped-define ENABLED-TABLES es-gp-emit-cond-pagto
&Scoped-define FIRST-ENABLED-TABLE es-gp-emit-cond-pagto
&Scoped-Define ENABLED-OBJECTS rt-key 
&Scoped-Define DISPLAYED-FIELDS es-gp-emit-cond-pagto.cod-cond-pag 
&Scoped-define DISPLAYED-TABLES es-gp-emit-cond-pagto
&Scoped-define FIRST-DISPLAYED-TABLE es-gp-emit-cond-pagto
&Scoped-Define DISPLAYED-OBJECTS fi-descricao 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS es-gp-emit-cond-pagto.cod-cond-pag 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
cod-emitente||y|es-gp-emit-cond-pagto.cod-emitente
cod-cond-pag||y|es-gp-emit-cond-pagto.cod-cond-pag
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "cod-emitente,cod-cond-pag"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE fi-descricao AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35.57 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 65.43 BY 2.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     es-gp-emit-cond-pagto.cod-cond-pag AT ROW 1.96 COL 14.72 COLON-ALIGNED WIDGET-ID 2
          LABEL "Cond de Pagto":R14
          VIEW-AS FILL-IN 
          SIZE 5 BY .88
     fi-descricao AT ROW 1.96 COL 20.14 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     rt-key AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: es-gp-emit-cond-pagto
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
         HEIGHT             = 2.96
         WIDTH              = 65.43.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}

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

/* SETTINGS FOR FILL-IN es-gp-emit-cond-pagto.cod-cond-pag IN FRAME f-main
   1 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN fi-descricao IN FRAME f-main
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

&Scoped-define SELF-NAME es-gp-emit-cond-pagto.cod-cond-pag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-gp-emit-cond-pagto.cod-cond-pag V-table-Win
ON F5 OF es-gp-emit-cond-pagto.cod-cond-pag IN FRAME f-main /* Cond de Pagto */
DO:
    {include/zoomvar.i &prog-zoom=adzoom\z01ad039.w
                       &campo=es-gp-emit-cond-pagto.cod-cond-pag    
                       &campozoom=cod-cond-pag} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-gp-emit-cond-pagto.cod-cond-pag V-table-Win
ON LEAVE OF es-gp-emit-cond-pagto.cod-cond-pag IN FRAME f-main /* Cond de Pagto */
DO:
  
    FOR FIRST cond-pagto FIELD(descricao)
        WHERE cond-pagto.cod-cond-pag = INPUT FRAME {&FRAME-NAME} es-gp-emit-cond-pagto.cod-cond-pag NO-LOCK:

        ASSIGN fi-descricao:SCREEN-VALUE = cond-pagto.descricao.

    END.
    IF NOT AVAIL cond-pagto THEN
        ASSIGN fi-descricao:SCREEN-VALUE = "".


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-gp-emit-cond-pagto.cod-cond-pag V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-gp-emit-cond-pagto.cod-cond-pag IN FRAME f-main /* Cond de Pagto */
DO:
  
    APPLY "F5" TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

    es-gp-emit-cond-pagto.cod-cond-pag:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.

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

  /* No Foreign keys are accepted by this SmartObject. */

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
  {src/adm/template/row-list.i "es-gp-emit-cond-pagto"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es-gp-emit-cond-pagto"}

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

    /* Code placed here will execute PRIOR to standard behavior. */
    {include/i-valid.i}
    
    /* Ponha na pi-validate todas as validaá‰es */
    /* N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */

    RUN pi-validate. 
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.

    IF AVAIL es-gp-emit-cond-pagto THEN DO:

        
        FIND FIRST emitente WHERE emitente.cod-emitente = es-gp-emit-cond-pagto.cod-emitente NO-LOCK NO-ERROR.

        /* ------ Projeto SFA ----- */
        IF (emitente.identific = 1 OR emitente.identific = 3) AND emitente.natureza = 2 THEN DO:

            
            FIND FIRST es-api-param NO-LOCK WHERE es-api-param.ind-tipo-trans = 2 /*---- Saida ----*/
                                              AND es-api-param.cd-tipo-integr = 2 /*---- Integraá∆o Cliente ------*/ NO-ERROR.
            IF AVAIL es-api-param THEN DO:

                IF (NOT CAN-FIND(FIRST es-api-export WHERE es-api-export.chave = emitente.cgc
                                                   AND es-api-export.ind-tipo-trans = es-api-param.ind-tipo-trans
                                                   AND es-api-export.cd-tipo-integr = es-api-param.cd-tipo-integr
                                                   AND es-api-export.ind-situacao < 2) ) THEN DO:

                    CREATE es-api-export-cli.
                    ASSIGN es-api-export-cli.cd-tipo-integr = es-api-param.cd-tipo-integr
                           es-api-export-cli.id-movto       = NEXT-VALUE(seq-export)
                           es-api-export-cli.cgc            = emitente.cgc
                           es-api-export-cli.data-movto     = NOW
                           es-api-export-cli.c-json         = ?.
            
                    CREATE es-api-export.
                    ASSIGN es-api-export.ind-tipo-trans = es-api-param.ind-tipo-trans
                           es-api-export.id-movto       = es-api-export-cli.id-movto
                           es-api-export.cd-tipo-integr = es-api-export-cli.cd-tipo-integr
                           es-api-export.chave          = es-api-export-cli.cgc
                           es-api-export.cod-status     = 0      /* ---- sem status ----*/
                           es-api-export.data-fim       = ?
                           es-api-export.data-inicio    = ?
                           es-api-export.data-movto     = NOW
                           es-api-export.ind-situacao   = 0       /*---- Pendente -----*/.
        
                    // RUN pi-processa (INPUT 2, INPUT 2).  
        
                END.
            END.
        END.
    END.
 
    
    
    /* Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    RUN dispatch IN THIS-PROCEDURE (INPUT 'create-record').

    FOR FIRST es-gp-emitente
        WHERE ROWID(es-gp-emitente) = v-row-parent NO-LOCK:

        ASSIGN es-gp-emit-cond-pagto.cod-emitente = es-gp-emitente.cod-emitente.

        RELEASE es-gp-emitente NO-ERROR.

    END.

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
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    RUN dispatch IN THIS-PROCEDURE (INPUT 'display-fields':U).

    IF AVAIL es-gp-emit-cond-pagto THEN DO:

        APPLY "LEAVE":U TO es-gp-emit-cond-pagto.cod-cond-pag IN FRAME {&FRAME-NAME}.

    END.
    ELSE 
        ASSIGN fi-descricao:SCREEN-VALUE = "".

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
    
    &IF DEFINED(ADM-CREATE-FIELDS) &THEN
    IF NOT adm-new-record THEN 
        DISABLE {&ADM-CREATE-FIELDS} WITH FRAME {&FRAME-NAME}.
    &ENDIF

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: N∆o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
  ainda n∆o foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /* Validaá∆o de dicion†rio */

    IF adm-new-record THEN DO:

        FOR FIRST es-gp-emitente
            WHERE ROWID(es-gp-emitente) = v-row-parent NO-LOCK:
            
            IF CAN-FIND(FIRST es-gp-emit-cond-pagto
                        WHERE es-gp-emit-cond-pagto.cod-emitente = es-gp-emitente.cod-emitente
                          AND es-gp-emit-cond-pagto.cod-cond-pag = INPUT FRAME {&FRAME-NAME} es-gp-emit-cond-pagto.cod-cond-pag NO-LOCK) THEN DO:
            
                RUN utp/ut-msgs.p (INPUT "show":U,
                                   INPUT 1,
                                   INPUT "Condiá∆o de Pagamento").
            
                APPLY "ENTRY":U TO es-gp-emit-cond-pagto.cod-cond-pag IN FRAME {&FRAME-NAME}.
            
                RETURN "ADM-ERROR":U.
                
            END.
            
            IF NOT CAN-FIND(FIRST cond-pagto
                            WHERE cond-pagto.cod-cond-pag = INPUT FRAME {&FRAME-NAME} es-gp-emit-cond-pagto.cod-cond-pag NO-LOCK) THEN DO:
               
                RUN utp/ut-msgs.p (INPUT "show":U,
                                   INPUT 2,
                                   INPUT "Condiá∆o de Pagamento").
            
                APPLY "ENTRY":U TO es-gp-emit-cond-pagto.cod-cond-pag IN FRAME {&FRAME-NAME}.
            
                RETURN "ADM-ERROR":U.
            
            END.
        END.
        IF NOT AVAIL es-gp-emitente THEN DO:
               
            RUN utp/ut-msgs.p (INPUT "show":U,
                               INPUT 15217,
                               INPUT "").
            
            APPLY "ENTRY":U TO es-gp-emit-cond-pagto.cod-cond-pag IN FRAME {&FRAME-NAME}.
            
            RETURN "ADM-ERROR":U.

        END.
    END.



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
  {src/adm/template/sndkycas.i "cod-emitente" "es-gp-emit-cond-pagto" "cod-emitente"}
  {src/adm/template/sndkycas.i "cod-cond-pag" "es-gp-emit-cond-pagto" "cod-cond-pag"}

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
  {src/adm/template/snd-list.i "es-gp-emit-cond-pagto"}

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

